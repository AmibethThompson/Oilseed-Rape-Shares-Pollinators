###################################################
## Code for Thompson et al. 2021, Insects
## "Oilseed Rape Shares Abundant and Generalized Pollinators with Its Co-Flowering Plant Species"
##
## Figure 2: NMDS

#load packages 
library(dplyr)
library(tidyr)
library(tibble)
library(bipartite)
library(MASS)
library(ggplot2)
library(ggpubr)

#load data 
dat <- read.csv("DataThompson2021_Insects.csv")

#create plant-pollinator matrix
dat2 <- dat %>% group_by(Plant, Morph) %>% summarise(Count=sum(Count))
mat<-pivot_wider(dat2, names_from = Morph, values_from = Count)
mat[is.na(mat)]<-0 #change NA to 0
mat<-column_to_rownames(mat, var="Plant")

#run NMDS
set.seed(2)
nmds <- metaMDS(mat, distance = "bray", k=2)
stressplot(nmds)
nmds$stress #stress level 

#extract data for plotting
data.scores_pl <- as.data.frame(nmds$points) %>% rownames_to_column(., var = "Plant")
data.scores_pl <- merge(data.scores_pl, plants, by = "Plant")

data.scores_pol <- as.data.frame(nmds$species) %>% rownames_to_column(., var = "Morph")
data.scores_pol <- merge(data.scores_pol, pollinator, by = "Morph")

#plotting 

fig2a <- ggplot(data.scores_pl, aes(x = MDS1, y = MDS2)) + 
  #geom_point(data = data.scores_pol, aes(x = MDS1, y = MDS2, col=Functional_group2), size = 2, stroke = 1.8,)+
  geom_point(size = 6, stroke = 1.8, aes(shape = fltyp))+ 
  geom_point(data=data.scores_pl[6,], aes(x = MDS1, y = MDS2), colour="red", size = 6) +
  #geom_text(aes(label=Plant),size= 2)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "none", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) + 
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + 
  scale_color_brewer(palette = "Dark2")+
  labs(x = "NMDS1",  y = "NMDS2")

fig2b <- ggplot(subset(data.scores_pl, MDS1 <2), aes(x = MDS1, y = MDS2)) + 
  geom_point(data = subset(data.scores_pol, MDS1 <2), aes(x = MDS1, y = MDS2, col=Functional_group2), size = 4, stroke = 1.8,)+
  geom_point(size = 6, stroke = 1.8, aes(shape = fltyp))+ 
  geom_point(data=data.scores_pl[6,], aes(x = MDS1, y = MDS2), colour="red", size = 6) +
  #geom_text(aes(label=Plant),size= 2)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + 
  scale_color_brewer(palette = "Dark2")+
  labs(x = "NMDS1", shape = "Flower structure", color ="Pollinator groups", y = "NMDS2")

fig2 <- ggarrange(fig2a, fig2b, nrow = 2)
ggsave(filename= "fig2_final.jpeg", device = "jpeg", plot = fig2, dpi = 400, width = 400, height = 400, units = "mm")

