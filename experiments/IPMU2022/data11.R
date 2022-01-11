# Number of clusters
nc <- length(unique(data11$class))
plot_hclust_comparison(data11, nc, mode = "sca")

# Create a subset
set.seed(10) # changes a lot depending on the seed
data11b <- caret::createDataPartition(
  data11$class,
  p = .05,
  list = F
)
data11b <- data11[data11b,]
data11_training <- data11b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data11b, nc, mode = "sca")
pcsca

data11_single <- hclust(dist(data11_training), method = "single")
data11_complete <- hclust(dist(data11_training), method = "complete")
data11_average <- hclust(dist(data11_training), method = "average")
data11_ward <- hclust(dist(data11_training), method = "ward.D")
data11_ward2 <- hclust(dist(data11_training), method = "ward.D2")
data11_mcquitty <- hclust(dist(data11_training), method = "mcquitty")
data11_median <- hclust(dist(data11_training), method = "median")
data11_centroid <- hclust(dist(data11_training), method = "centroid")

######## PLURALITY #############################################################

data11_sc_plurality <- mc_hclust(data11_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data11_sca_plurality <- mc_hclust(data11_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data11_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data11_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data11_sc_tapproval <- mc_hclust(data11_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data11_sca_tapproval <- mc_hclust(data11_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data11_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data11_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data11_sc_borda <- mc_hclust(data11_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data11_sca_borda <- mc_hclust(data11_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data11_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data11_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data11_single,
     data11_complete,
     data11_average,
     data11_ward,
     data11_ward2,
     data11_mcquitty,
     data11_median,
     data11_centroid,
     # aggregation methods
     data11_sc_plurality,
     data11_sca_plurality,
     data11_sc_tapproval,
     data11_sca_tapproval,
     data11_sc_borda,
     data11_sca_borda,
     file = "experiments/results/results_data11.RData")

