# Number of clusters
nc <- length(unique(data16$class))
plot_hclust_comparison(data16, nc, mode = "sca")

# Create a subset
set.seed(3) # changes a lot depending on the seed
data16b <- caret::createDataPartition(
  data16$class,
  p = .15,
  list = F
)
data16b <- data16[data16b,]
data16_training <- data16b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data16b, nc, mode = "sca")
pcsca

data16_single <- hclust(dist(data16_training), method = "single")
data16_complete <- hclust(dist(data16_training), method = "complete")
data16_average <- hclust(dist(data16_training), method = "average")
data16_ward <- hclust(dist(data16_training), method = "ward.D")
data16_ward2 <- hclust(dist(data16_training), method = "ward.D2")
data16_mcquitty <- hclust(dist(data16_training), method = "mcquitty")
data16_median <- hclust(dist(data16_training), method = "median")
data16_centroid <- hclust(dist(data16_training), method = "centroid")

######## PLURALITY #############################################################

data16_sc_plurality <- mc_hclust(data16_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data16_sca_plurality <- mc_hclust(data16_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data16_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data16_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data16_sc_tapproval <- mc_hclust(data16_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data16_sca_tapproval <- mc_hclust(data16_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data16_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data16_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data16_sc_borda <- mc_hclust(data16_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data16_sca_borda <- mc_hclust(data16_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data16_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data16_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data16_single,
     data16_complete,
     data16_average,
     data16_ward,
     data16_ward2,
     data16_mcquitty,
     data16_median,
     data16_centroid,
     # aggregation methods
     data16_sc_plurality,
     data16_sca_plurality,
     data16_sc_tapproval,
     data16_sca_tapproval,
     data16_sc_borda,
     data16_sca_borda,
     file = "experiments/IPMU2022/results/results_data16.RData")

