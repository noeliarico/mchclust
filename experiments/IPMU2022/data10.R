# Number of clusters
nc <- length(unique(data10$class))
plot_hclust_comparison(data10, nc, mode = "sca")

# Create a subset
set.seed(3) # changes a lot depending on the seed
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

data10_single <- hclust(dist(data10_training), method = "single")
data10_complete <- hclust(dist(data10_training), method = "complete")
data10_average <- hclust(dist(data10_training), method = "average")
data10_ward <- hclust(dist(data10_training), method = "ward.D")
data10_ward2 <- hclust(dist(data10_training), method = "ward.D2")
data10_mcquitty <- hclust(dist(data10_training), method = "mcquitty")
data10_median <- hclust(dist(data10_training), method = "median")
data10_centroid <- hclust(dist(data10_training), method = "centroid")

######## PLURALITY #############################################################

data10_sc_plurality <- mc_hclust(data10_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data10_sca_plurality <- mc_hclust(data10_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data10_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data10_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data10_sc_tapproval <- mc_hclust(data10_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data10_sca_tapproval <- mc_hclust(data10_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data10_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data10_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data10_sc_borda <- mc_hclust(data10_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data10_sca_borda <- mc_hclust(data10_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data10_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data10_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data10_single,
     data10_complete,
     data10_average,
     data10_ward,
     data10_ward2,
     data10_mcquitty,
     data10_median,
     data10_centroid,
     # aggregation methods
     data10_sc_plurality,
     data10_sca_plurality,
     data10_sc_tapproval,
     data10_sca_tapproval,
     data10_sc_borda,
     data10_sca_borda,
     file = "experiments/IPMU2022/results/results_data10.RData")

