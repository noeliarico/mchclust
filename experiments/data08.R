# Number of clusters
nc <- length(unique(data08))
plot_hclust_comparison(data08, nc, mode = "sca")

# Create a subset
set.seed(3)
data08b <- caret::createDataPartition(
  data08$class,
  p = .16,
  list = F
)
data08b <- data08[data08b,]
data08_training <- data08b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data08b, nc, mode = "sca")
pcsca

data08_s <- hclust(dist(data08_training), method = "single")
data08_c <- hclust(dist(data08_training), method = "complete")
data08_a <- hclust(dist(data08_training), method = "average")

######## PLURALITY #############################################################

data08_sc_plurality <- mc_hclust(data08_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data08_sca_plurality <- mc_hclust(data08_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

plot_mchclust_tiles(data08_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data08_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data08_sc_tapproval <- mc_hclust(data08_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data08_sca_tapproval <- mc_hclust(data08_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

plot_mchclust_tiles(data08_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data08_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data08_sc_borda <- mc_hclust(data08_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data08_sca_borda <- mc_hclust(data08_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

################################################################################

# Save results

evaluate_results

save(data08_sc_plurality, data08_sca_plurality,
     data08_sc_tapproval, data08_sca_tapproval,
     data08_sc_borda, data08_sca_borda,
     file = "experiments/results/results_data08.RData")
