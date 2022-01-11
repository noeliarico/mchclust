# Number of clusters
nc <- length(unique(data09$class))
plot_hclust_comparison(data09, nc, mode = "sca")

# Create a subset
set.seed(10) # changes a lot depending on the seed
data09b <- caret::createDataPartition(
  data09$class,
  p = .03,
  list = F
)
data09b <- data09[data09b,]
data09_training <- data09b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data09b, nc, mode = "sca")
pcsca

data09_s <- hclust(dist(data09_training), method = "single")
data09_c <- hclust(dist(data09_training), method = "complete")
data09_a <- hclust(dist(data09_training), method = "average")

######## PLURALITY #############################################################

data09_sc_plurality <- mc_hclust(data09_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data09_sca_plurality <- mc_hclust(data09_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

plot_mchclust_tiles(data09_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data09_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data09_sc_tapproval <- mc_hclust(data09_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data09_sca_tapproval <- mc_hclust(data09_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

plot_mchclust_tiles(data09_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data09_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data09_sc_borda <- mc_hclust(data09_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data09_sca_borda <- mc_hclust(data09_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

################################################################################

# Save results

save(data09_sc_plurality, data09_sca_plurality,
     data09_sc_tapproval, data09_sca_tapproval,
     data09_sc_borda, data09_sca_borda,
     file = "experiments/results/results_data09.RData")

