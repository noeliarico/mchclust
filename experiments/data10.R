# Number of clusters
nc <- length(unique(data10$class))
plot_hclust_comparison(data10, nc, mode = "sca")

# Create a subset
set.seed(10) # changes a lot depending on the seed
data10b <- caret::createDataPartition(
  data10$class,
  p = .08,
  list = F
)
data10b <- data10[data10b,]
data10_training <- data10b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data10b, nc, mode = "sca")
pcsca

data10_s <- hclust(dist(data10_training), method = "single")
data10_c <- hclust(dist(data10_training), method = "complete")
data10_a <- hclust(dist(data10_training), method = "average")

######## PLURALITY #############################################################

data10_sc_plurality <- mc_hclust(data10_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data10_sca_plurality <- mc_hclust(data10_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

plot_mchclust_tiles(data10_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data10_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data10_sc_tapproval <- mc_hclust(data10_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data10_sca_tapproval <- mc_hclust(data10_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

plot_mchclust_tiles(data10_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data10_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data10_sc_borda <- mc_hclust(data10_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data10_sca_borda <- mc_hclust(data10_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

################################################################################

# Save results

save(data10_sc_plurality, data10_sca_plurality,
     data10_sc_tapproval, data10_sca_tapproval,
     data10_sc_borda, data10_sca_borda,
     file = "experiments/results/results_data10.RData")

