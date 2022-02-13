# Number of clusters
nc <- length(unique(data14$class))
plot_hclust_comparison(data14, nc, mode = "sca")

# Create a subset
set.seed(1) # changes a lot depending on the seed
data14b <- caret::createDataPartition(
  data14$class,
  p = .18,
  list = F
)
data14b <- data14[data14b,]
data14_training <- data14b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data14b, nc, mode = "sca")
pcsca

data14_single <- hclust(dist(data14_training), method = "single")
data14_complete <- hclust(dist(data14_training), method = "complete")
data14_average <- hclust(dist(data14_training), method = "average")
data14_ward <- hclust(dist(data14_training), method = "ward.D")
data14_ward2 <- hclust(dist(data14_training), method = "ward.D2")
data14_mcquitty <- hclust(dist(data14_training), method = "mcquitty")
data14_median <- hclust(dist(data14_training), method = "median")
data14_centroid <- hclust(dist(data14_training), method = "centroid")

######## PLURALITY #############################################################

data14_sc_plurality <- mc_hclust(data14_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data14_sca_plurality <- mc_hclust(data14_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data14_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data14_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data14_sc_tapproval <- mc_hclust(data14_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data14_sca_tapproval <- mc_hclust(data14_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data14_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data14_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data14_sc_borda <- mc_hclust(data14_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data14_sca_borda <- mc_hclust(data14_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data14_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data14_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data14_single,
     data14_complete,
     data14_average,
     data14_ward,
     data14_ward2,
     data14_mcquitty,
     data14_median,
     data14_centroid,
     # aggregation methods
     data14_sc_plurality,
     data14_sca_plurality,
     data14_sc_tapproval,
     data14_sca_tapproval,
     data14_sc_borda,
     data14_sca_borda,
     file = "experiments/IPMU2022/results/results_data14.RData")

