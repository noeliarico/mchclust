# Number of clusters
nc <- length(unique(data07$class))
plot_hclust_comparison(data07, nc, mode = "sca")

# Create a subset
set.seed(2) # changes a lot depending on the seed
data07b <- caret::createDataPartition(
  data07$class,
  p = .08,
  list = F
)
data07b <- data07[data07b,]
data07_training <- data07b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data07b, nc, mode = "sca")
pcsca

data07_single <- hclust(dist(data07_training), method = "single")
data07_complete <- hclust(dist(data07_training), method = "complete")
data07_average <- hclust(dist(data07_training), method = "average")
data07_ward <- hclust(dist(data07_training), method = "ward.D")
data07_ward2 <- hclust(dist(data07_training), method = "ward.D2")
data07_mcquitty <- hclust(dist(data07_training), method = "mcquitty")
data07_median <- hclust(dist(data07_training), method = "median")
data07_centroid <- hclust(dist(data07_training), method = "centroid")

######## PLURALITY #############################################################

data07_sc_plurality <- mc_hclust(data07_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data07_sca_plurality <- mc_hclust(data07_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data07_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data07_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data07_sc_tapproval <- mc_hclust(data07_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data07_sca_tapproval <- mc_hclust(data07_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data07_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data07_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data07_sc_borda <- mc_hclust(data07_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data07_sca_borda <- mc_hclust(data07_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data07_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data07_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data07_single,
     data07_complete,
     data07_average,
     data07_ward,
     data07_ward2,
     data07_mcquitty,
     data07_median,
     data07_centroid,
     # aggregation methods
     data07_sc_plurality,
     data07_sca_plurality,
     data07_sc_tapproval,
     data07_sca_tapproval,
     data07_sc_borda,
     data07_sca_borda,
     file = "experiments/IPMU2022/results/results_data07.RData")

