# Number of clusters
nc <- length(unique(data15$class))
plot_hclust_comparison(data15, nc, mode = "sca")

# Create a subset
set.seed(1) # changes a lot depending on the seed
data15b <- caret::createDataPartition(
  data15$class,
  p = .2,
  list = F
)
data15b <- data15[data15b,]
data15_training <- data15b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data15b, nc, mode = "sca")
pcsca

data15_single <- hclust(dist(data15_training), method = "single")
data15_complete <- hclust(dist(data15_training), method = "complete")
data15_average <- hclust(dist(data15_training), method = "average")
data15_ward <- hclust(dist(data15_training), method = "ward.D")
data15_ward2 <- hclust(dist(data15_training), method = "ward.D2")
data15_mcquitty <- hclust(dist(data15_training), method = "mcquitty")
data15_median <- hclust(dist(data15_training), method = "median")
data15_centroid <- hclust(dist(data15_training), method = "centroid")

######## PLURALITY #############################################################

data15_sc_plurality <- mc_hclust(data15_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data15_sca_plurality <- mc_hclust(data15_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data15_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data15_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data15_sc_tapproval <- mc_hclust(data15_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data15_sca_tapproval <- mc_hclust(data15_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data15_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data15_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data15_sc_borda <- mc_hclust(data15_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data15_sca_borda <- mc_hclust(data15_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data15_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data15_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data15_single,
     data15_complete,
     data15_average,
     data15_ward,
     data15_ward2,
     data15_mcquitty,
     data15_median,
     data15_centroid,
     # aggregation methods
     data15_sc_plurality,
     data15_sca_plurality,
     data15_sc_tapproval,
     data15_sca_tapproval,
     data15_sc_borda,
     data15_sca_borda,
     file = "experiments/IPMU2022/results/results_data15.RData")

