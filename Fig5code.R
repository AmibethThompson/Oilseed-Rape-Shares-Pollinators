###################################################
## Code for Thompson et al. 2021, Insects
## "Oilseed Rape Shares Abundant and Generalized Pollinators with Its Co-Flowering Plant Species"
##
## Figure 5: Bar graph modules

#load packages 
library(dplyr)
library(tibble)
library(bipartite)
library(ggplot2)
library(ggpubr)

#load data 
dat <- read.csv("DataThompson2021_Insects.csv")

#create plant-pollinator matrix
dat2 <- dat %>% group_by(Plant, Morph) %>% summarise(Count=sum(Count))
mat<-pivot_wider(dat2, names_from = Morph, values_from = Count)
mat[is.na(mat)]<-0 #change NA to 0
mat<-column_to_rownames(mat, var="Plant")

#calculate modularity
set.seed(2)
mod <- metaComputeModules(mat)

nrow(mod@modules)-1 #number of modules
mod@likelihood #modularity 

modules <- as.data.frame(mod@modules[-1,-c(1,2)]) #removes empty rows and columns
colnames(modules) <- c(rownames(mat), colnames(mat)) 

mod_dat <- data.frame()
for(i in 1:nrow(modules)) 
  for(j in 1:ncol(modules))
  {
    if(modules[i,j] > 0)
    {
      mod_dat[j, "species"] <- colnames(modules[j])
      mod_dat[j, "modules"] <- rownames(modules)[i]
    }
  }

mod_dat$type <- c(rep("Plant",nrow(mat)),rep("Pollinator",ncol(mat)))
plants$count <- rowSums(mat)
pollinator$count <- colSums(mat)

colnames(plants) <- c("species", "funt", "count")
colnames(pollinator) <- c("species", "funt","count")
funt <- rbind(plants, pollinator)
mod_fun <- merge(mod_dat, funt, by = "species")
mod_fun$modules <- as.numeric(mod_fun$modules)

#plotting 

fig5a <- ggplot(subset(mod_fun, type == "Plant"), aes(x=modules, y=count,fill=funt)) + geom_bar(stat = "identity") + 
  theme_classic() + scale_fill_brewer(palette = "Set1") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = 1:16) +
  labs(x = "Modules", y = "Number of Interactions", fill ="Flower structure", title = "a) Flower structure in modules")


fig5b <- ggplot(subset(mod_fun, type == "Pollinator"), aes(x=modules, y=count,fill=funt)) + geom_bar(stat = "identity") + 
  theme_classic() + scale_fill_brewer(palette = "Dark2") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = 1:16) +
  labs(x = "Modules", y = "Number of Interactions", fill ="Pollinator groups", title = "b) Pollinator groups in modules") 


fig5 <- ggarrange(fig5a, fig5b, ncol = 2)
ggsave(filename= "fig5_final.jpeg", device = "jpeg", plot = fig5, dpi = 400, width = 400, height = 200, units = "mm")

