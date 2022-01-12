# Number of clusters
nc <- length(unique(data06$class))
plot_hclust_comparison(data06, nc, mode = "sca")

# Create a subset
set.seed(2) # changes a lot depending on the seed
data06b <- caret::createDataPartition(
  data06$class,
  p = .03,
  list = F
)
data06b <- data06[data06b,]
data06_training <- data06b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data06b, nc, mode = "sca")
pcsca

data06_single <- hclust(dist(data06_training), method = "single")
data06_complete <- hclust(dist(data06_training), method = "complete")
data06_average <- hclust(dist(data06_training), method = "average")
data06_ward <- hclust(dist(data06_training), method = "ward.D")
data06_ward2 <- hclust(dist(data06_training), method = "ward.D2")
data06_mcquitty <- hclust(dist(data06_training), method = "mcquitty")
data06_median <- hclust(dist(data06_training), method = "median")
data06_centroid <- hclust(dist(data06_training), method = "centroid")

######## PLURALITY #############################################################

data06_sc_plurality <- mc_hclust(data06_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data06_sca_plurality <- mc_hclust(data06_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data06_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data06_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data06_sc_tapproval <- mc_hclust(data06_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data06_sca_tapproval <- mc_hclust(data06_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data06_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data06_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data06_sc_borda <- mc_hclust(data06_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data06_sca_borda <- mc_hclust(data06_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data06_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data06_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data06_single,
     data06_complete,
     data06_average,
     data06_ward,
     data06_ward2,
     data06_mcquitty,
     data06_median,
     data06_centroid,
     # aggregation methods
     data06_sc_plurality,
     data06_sca_plurality,
     data06_sc_tapproval,
     data06_sca_tapproval,
     data06_sc_borda,
     data06_sca_borda,
     file = "experiments/IPMU2022/results/results_data06.RData")

