###################################################
## Code for Thompson et al. 2021, Insects
## "Oilseed Rape Shares Abundant and Generalized Pollinators with Its Co-Flowering Plant Species"
##
## Figure 3: Bray-Curtis distance null model

#load packages 
library(dplyr)
library(tibble)
library(vegan)
library(ggplot2)
library(matrixStats)

#load data 
dat <- read.csv("DataThompson2021_Insects.csv")

#create plant-pollinator matrix
dat2 <- dat %>% group_by(Plant, Morph) %>% summarise(Count=sum(Count))
mat<-pivot_wider(dat2, names_from = Morph, values_from = Count)
mat[is.na(mat)]<-0 #change NA to 0
mat<-column_to_rownames(mat, var="Plant")

# Function to shuffle abundances with given probabilities
shuffle_given_prob <- function(x, seed, prob){
  s <- sum(x)
  set.seed(seed)
  res <- rmultinom(n = 1, size = s, prob = prob)
  return(res)
}

# Observed bray Curtis distance of Brassica napus
dist_df <- vegdist(mat, method = "bray") %>% 
  as.matrix %>% 
  .[, "Brassica_napus"] %>% 
  as.data.frame
dist_df$sp <- rownames(dist_df)
colnames(dist_df)[1] <- "d"

N <- 1000
bn_dist_2 <- matrix(nrow = dim(mat)[1],
                    ncol = N)
rownames(bn_dist_2) <- rownames(mat)

# What are the most probable plants
prob <- rowSums(mat)/sum(mat)


for (i in 1:N){
  shuffled_2 <- sapply(mat, shuffle_given_prob, seed = i, prob = prob)
  rownames(shuffled_2) <- rownames(mat)
  bn_dist_2[, i] <- vegdist(shuffled_2, method = "bray") %>% 
    as.matrix %>% 
    .[,"Brassica_napus"]
}

dist_df <- dist_df %>% 
  mutate(sim_d_avg_2 = rowMeans(bn_dist_2, na.rm=TRUE),
         ci_low_2 = rowQuantiles(bn_dist_2, probs = 0.025, na.rm=TRUE),
         ci_up_2 = rowQuantiles(bn_dist_2, probs = 0.975, na.rm=TRUE))

null_model_2 <- ggplot(data = subset(dist_df, sp != "Brassica_napus")) + theme_classic() +
  geom_point(aes(x = reorder(sp, -d),
                 y = d)) +
  geom_point(aes(x = reorder(sp, -d),
                 y = sim_d_avg_2),
             col = "red") +
  geom_errorbar(aes(x = reorder(sp, -d),
                    ymax = ci_up_2, 
                    ymin = ci_low_2), 
                size  = .4, 
                width = .15, 
                linetype = "solid",
                color = "red") +
  labs(x = "Plants",
       y = "Bray-Curtis dist from OSR") +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))
print(null_model_2)

ggsave(filename= "fig3_final.jpeg", device = "jpeg", plot = null_model_2, dpi = 400, width = 300, height = 200, units = "mm")
