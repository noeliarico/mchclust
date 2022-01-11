# Number of clusters
nc <- length(unique(data05))
plot_hclust_comparison(data05, nc, mode = "sca")

# Create a subset
set.seed(3)
data05b <- caret::createDataPartition(
  data05$class,
  p = .4,
  list = F
)
data05b <- data05[data05b,]
data05_training <- data05b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data05b, nc, mode = "sca")
pcsca

data05_s <- hclust(dist(data05_training), method = "single")
data05_c <- hclust(dist(data05_training), method = "complete")
data05_a <- hclust(dist(data05_training), method = "average")

######## PLURALITY #############################################################

data05_sc_plurality <- mc_hclust(data05_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data05_sca_plurality <- mc_hclust(data05_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

plot_mchclust_tiles(data05_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data05_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data05_sc_tapproval <- mc_hclust(data05_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data05_sca_tapproval <- mc_hclust(data05_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

plot_mchclust_tiles(data05_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data05_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data05_sc_borda <- mc_hclust(data05_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data05_sca_borda <- mc_hclust(data05_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

################################################################################

# Save results

save(data05_sc_plurality, data05_sca_plurality,
     data05_sc_tapproval, data05_sca_tapproval,
     data05_sc_borda, data05_sca_borda,
     file = "experiments/results/results_data05.RData")
