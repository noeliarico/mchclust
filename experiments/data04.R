# Number of clusters
nc <- length(unique(data04))
plot_hclust_comparison(data04, nc, mode = "sca")

# Create a subset
set.seed(3)
data04b <- caret::createDataPartition(
  data04$class,
  p = .2,
  list = F
)
data04b <- data04[data04b,]
data04_training <- data04b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data04b, nc, mode = "sca")
pcsca

data04_s <- hclust(dist(data04_training), method = "single")
data04_c <- hclust(dist(data04_training), method = "complete")
data04_a <- hclust(dist(data04_training), method = "average")

######## PLURALITY #############################################################

data04_sc_plurality <- mc_hclust(data04_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data04_sca_plurality <- mc_hclust(data04_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

plot_mchclust_tiles(data04_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data04_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data04_sc_tapproval <- mc_hclust(data04_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data04_sca_tapproval <- mc_hclust(data04_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

plot_mchclust_tiles(data04_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data04_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data04_sc_borda <- mc_hclust(data04_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data04_sca_borda <- mc_hclust(data04_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

################################################################################

# Save results

evaluate_results

save(data04_sc_plurality, data04_sca_plurality,
     data04_sc_tapproval, data04_sca_tapproval,
     data04_sc_borda, data04_sca_borda,
     file = "experiments/results/results_data04.RData")
