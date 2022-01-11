# Number of clusters
nc <- length(unique(data02))
plot_hclust_comparison(data02, nc, mode = "sca")

# Create a subset
set.seed(3)
data02b <- caret::createDataPartition(
  data02$class,
  p = .2,
  list = F
)
data02b <- data02[data02b,]
data02_training <- data02b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data02b, nc, mode = "sca")
pcsca

data02_s <- hclust(dist(data02_training), method = "single")
data02_c <- hclust(dist(data02_training), method = "complete")
data02_a <- hclust(dist(data02_training), method = "average")

######## PLURALITY #############################################################

data02_sc_plurality <- mc_hclust(data02_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data02_sca_plurality <- mc_hclust(data02_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

plot_mchclust_tiles(data02_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data02_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data02_sc_tapproval <- mc_hclust(data02_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data02_sca_tapproval <- mc_hclust(data02_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

plot_mchclust_tiles(data02_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data02_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data02_sc_borda <- mc_hclust(data02_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data02_sca_borda <- mc_hclust(data02_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

################################################################################

# Save results

save(data02_sc_plurality, data02_sca_plurality,
     data02_sc_tapproval, data02_sca_tapproval,
     data02_sc_borda, data02_sca_borda,
     file = "experiments/results/results_data02.RData")
