###################################################
## Code for Thompson et al. 2021, Insects
## "Oilseed Rape Shares Abundant and Generalized Pollinators with Its Co-Flowering Plant Species"
##
## Figure 6: C- and Z-values

#load packages 
library(dplyr)
library(tibble)
library(bipartite)
library(ggplot2)
library(ggpubr)
library(ggrepel)

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

#extract observed cz values
czobs_pl <- czvalues(mod, weighted = F, level = "lower")
czobs_pol <- czvalues(mod, weighted = F, level = "higher")

#calculate cz values for pollinators from null models
set.seed(2)
nulls <- nullmodel(mat, N=1000)
null.mod.list <- sapply(nulls, computeModules)
null.cz_pol <- lapply(null.mod.list, czvalues)
# compute 95% for Pollinators
null.cpol <- sapply(null.cz_pol, function(x) x$c) # c-values across all pollinators in nulls
c_pol <- quantile(null.cpol, 0.95) 
null.zpol <- sapply(null.cz_pol, function(x) x$z) # z-values across all pollinators in nulls
z_pol <- quantile(null.zpol, 0.95, na.rm = T) 

czval_pol <- t(rbind(czobs_pol[[1]], czobs_pol[[2]])) 
colnames(czval_pol) <- c("c", "z")
czval_pol <- as.data.frame(czval_pol)
czval_pol$species <- rownames(czval_pol)
czval_pol<- merge(czval_pol, pollinator, by= 'species')

fig6b <- ggplot(czval_pol, aes(x=c, y= z)) + geom_point(size=2, stroke= 1.8, aes(color= funt, shape=funt)) +  
  theme_classic() + scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) + 
  geom_vline(aes(xintercept=c_pol)) + geom_hline(aes(yintercept=z_pol)) + scale_color_brewer(palette = "Dark2") + 
  ggtitle("b) Pollinators") + labs(x="Among-module connectivity, c", y= "Within-module degree, z", 
                                   color="Pollinator groups", shape="Pollinator groups") +
  geom_text_repel(data = subset(czval_pol, c >= as.numeric(c_pol) | z >= as.numeric(z_pol)), aes(c,z,label=species))


#calculate cz values for plants from null models
null.cz_pl <- list()
for (i in 1:length(null.mod.list)) {
  
  low_val <- czvalues(null.mod.list[[i]], weighted = F, level = "lower" )
  null.cz_pl[i] <- list(low_val)
  
}
# compute 95% for Plants
null.cpl <- sapply(null.cz_pl, function(x) x$c) # c-values across all plants in nulls
c_pl <- quantile(null.cpl, 0.95) 
null.zpl <- sapply(null.cz_pl, function(x) x$z) # z-values across all plants in nulls
z_pl <- quantile(null.zpl, 0.95, na.rm = T) 


czval_pl <- t(rbind(czobs_pl[[1]], czobs_pl[[2]])) 
colnames(czval_pl) <- c("c", "z")
czval_pl <- as.data.frame(czval_pl)
czval_pl$species <- rownames(czval_pl)
czval_pl<- merge(czval_pl, plants, by= 'species')

fig6a <- ggplot(czval_pl, aes(x=c, y= z)) + geom_point(size = 2, stroke = 1.8, aes(color= funt, shape=funt)) + 
  theme_classic() + scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + 
  geom_vline(aes(xintercept=c_pl)) + geom_hline(aes(yintercept=z_pl)) + scale_color_brewer(palette = "Set1") +
  ggtitle("a) Plants") + labs(x="Among-module connectivity, c", y="Within-module degree, z", 
                              color="Flower structure", shape="Flower structure") +
  geom_text_repel(data = subset(czval_pl, c >= as.numeric(c_pl) | z >= as.numeric(z_pl)), aes(c,z,label=species))

ggarrange(fig6a, fig6b, nrow = 2)
