# Number of clusters
nc <- length(unique(data09$class))
plot_hclust_comparison(data09, nc, mode = "sca")

# Create a subset
set.seed(3) # changes a lot depending on the seed
data09b <- caret::createDataPartition(
  data09$class,
  p = .04,
  list = F
)
data09b <- data09[data09b,]
data09_training <- data09b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data09b, nc, mode = "sca")
pcsca

data09_single <- hclust(dist(data09_training), method = "single")
data09_complete <- hclust(dist(data09_training), method = "complete")
data09_average <- hclust(dist(data09_training), method = "average")
data09_ward <- hclust(dist(data09_training), method = "ward.D")
data09_ward2 <- hclust(dist(data09_training), method = "ward.D2")
data09_mcquitty <- hclust(dist(data09_training), method = "mcquitty")
data09_median <- hclust(dist(data09_training), method = "median")
data09_centroid <- hclust(dist(data09_training), method = "centroid")

######## PLURALITY #############################################################

data09_sc_plurality <- mc_hclust(data09_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data09_sca_plurality <- mc_hclust(data09_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data09_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data09_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data09_sc_tapproval <- mc_hclust(data09_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data09_sca_tapproval <- mc_hclust(data09_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data09_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data09_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data09_sc_borda <- mc_hclust(data09_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data09_sca_borda <- mc_hclust(data09_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data09_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data09_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data09_single,
     data09_complete,
     data09_average,
     data09_ward,
     data09_ward2,
     data09_mcquitty,
     data09_median,
     data09_centroid,
     # aggregation methods
     data09_sc_plurality,
     data09_sca_plurality,
     data09_sc_tapproval,
     data09_sca_tapproval,
     data09_sc_borda,
     data09_sca_borda,
     file = "experiments/IPMU2022/results/results_data09.RData")

