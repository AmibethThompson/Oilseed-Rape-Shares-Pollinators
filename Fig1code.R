###################################################
## Code for Thompson et al. 2021, Insects
## "Oilseed Rape Shares Abundant and Generalized Pollinators with Its Co-Flowering Plant Species"
##
## Figure 1: Plant-pollinator network

#load packages 
library(dplyr)
library(tidyr)
library(tibble)

#load data 
dat <- read.csv("DataThompson2021_Insects.csv")

#create plant-pollinator matrix
dat2 <- dat %>% group_by(Functional_group2, Plant) %>% summarise(Count=sum(Count))
mat2<-pivot_wider(dat2, names_from = Plant, values_from = Count)
mat2[is.na(mat2)]<-0 #change NA to 0
mat2<-column_to_rownames(mat2, var="Functional_group2")

jpeg(filename = "fig1_final.jpeg", bg = "transparent", width = 400, height = 200, units = "mm", res = 600, pointsize = 12)
plotweb(mat2, text.rot=90,y.lim=c(-0.7,3.3), labsize=1,
        col.high=ifelse(colnames(mat2) == "Brassica_napus",adjustcolor('black'),
                        ifelse(colnames(mat2) == "Taraxacum_officinale",adjustcolor('grey10', alpha.f = 0.5),
                               ifelse(colnames(mat2)== "Crataegus_monogyna",adjustcolor('grey10', alpha.f = 0.5),
                                      ifelse(colnames(mat2)== "Lamium_purpureum",adjustcolor('grey10', alpha.f = 0.5),
                                             ifelse(colnames(mat2)== "Lamium_album",adjustcolor('grey10', alpha.f = 0.5),
                                                    adjustcolor('grey', alpha.f = 0.5)))))),
        col.interaction = ifelse(colnames(mat2)== "Brassica_napus",adjustcolor('black'),
                                 ifelse(colnames(mat2) == "Taraxacum_officinale",adjustcolor('grey10', alpha.f = 0.5),
                                        ifelse(colnames(mat2)== "Crataegus_monogyna",adjustcolor('grey10', alpha.f = 0.5),
                                               ifelse(colnames(mat2)== "Lamium_purpureum",adjustcolor('grey10', alpha.f = 0.5),
                                                      ifelse(colnames(mat2)== "Lamium_album",adjustcolor('grey10', alpha.f = 0.5),      
                                                             adjustcolor('grey80', alpha.f = 0.5)))))), 
        col.low = 'grey', bor.col.interaction = NA)
dev.off()
