###################################################
## Code for Thompson et al. 2021, Insects
## "Oilseed Rape Shares Abundant and Generalized Pollinators with Its Co-Flowering Plant Species"
##
## Figure 4: Modular network

#load packages 
library(dplyr)
library(tibble)
library(bipartite)
library(ggplot2)

#load data 
dat <- read.csv("DataThompson2021_Insects.csv")

#create plant-pollinator matrix
dat_ID <- dat %>% group_by(Pl_ID2, Pol_ID2) %>% summarise(Count=sum(Count))
mat_ID<-pivot_wider(dat_ID, names_from = Pol_ID2, values_from = Count)
mat_ID[is.na(mat_ID)]<-0 #change NA to 0
mat_ID<-column_to_rownames(mat_ID, var="Pl_ID2")

#calculate modularity
set.seed(2)
mod_ID <- metaComputeModules(mat_ID)

#plot network
jpeg(filename = "fig4_final.jpeg", bg = "transparent", width = 400, height = 200, units = "mm", res = 600, pointsize = 12)
plotModuleWeb(mod_ID, labsize = 0.4, rank = TRUE, xlabel = "Pollinator Species", ylabel = "Plant Species")
dev.off()