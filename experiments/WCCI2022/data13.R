# Number of clusters
nc <- length(unique(data13$class))
plot_hclust_comparison(data13, nc, mode = "sca")

# Create a subset
set.seed(1) # changes a lot depending on the seed
data13b <- caret::createDataPartition(
  data13$class,
  p = .15,
  list = F
)
data13b <- data13[data13b,]
data13_training <- data13b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data13b, nc, mode = "sca")
pcsca

data13_single <- hclust(dist(data13_training), method = "single")
data13_complete <- hclust(dist(data13_training), method = "complete")
data13_average <- hclust(dist(data13_training), method = "average")
data13_ward <- hclust(dist(data13_training), method = "ward.D")
data13_ward2 <- hclust(dist(data13_training), method = "ward.D2")
data13_mcquitty <- hclust(dist(data13_training), method = "mcquitty")
data13_median <- hclust(dist(data13_training), method = "median")
data13_centroid <- hclust(dist(data13_training), method = "centroid")

######## PLURALITY #############################################################

data13_sc_plurality <- mc_hclust(data13_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data13_sca_plurality <- mc_hclust(data13_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data13_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data13_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data13_sc_tapproval <- mc_hclust(data13_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data13_sca_tapproval <- mc_hclust(data13_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data13_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data13_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data13_sc_borda <- mc_hclust(data13_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data13_sca_borda <- mc_hclust(data13_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data13_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data13_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data13_single,
     data13_complete,
     data13_average,
     data13_ward,
     data13_ward2,
     data13_mcquitty,
     data13_median,
     data13_centroid,
     # aggregation methods
     data13_sc_plurality,
     data13_sca_plurality,
     data13_sc_tapproval,
     data13_sca_tapproval,
     data13_sc_borda,
     data13_sca_borda,
     file = "experiments/IPMU2022/results/results_data13.RData")

