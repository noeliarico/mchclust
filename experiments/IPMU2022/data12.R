# Number of clusters
nc <- length(unique(data12$class))
plot_hclust_comparison(data12, nc, mode = "sca")

# Create a subset
set.seed(2) # changes a lot depending on the seed
data12b <- caret::createDataPartition(
  data12$class,
  p = .05,
  list = F
)
data12b <- data12[data12b,]
data12_training <- data12b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data12b, nc, mode = "sca")
pcsca

data12_single <- hclust(dist(data12_training), method = "single")
data12_complete <- hclust(dist(data12_training), method = "complete")
data12_average <- hclust(dist(data12_training), method = "average")
data12_ward <- hclust(dist(data12_training), method = "ward.D")
data12_ward2 <- hclust(dist(data12_training), method = "ward.D2")
data12_mcquitty <- hclust(dist(data12_training), method = "mcquitty")
data12_median <- hclust(dist(data12_training), method = "median")
data12_centroid <- hclust(dist(data12_training), method = "centroid")

######## PLURALITY #############################################################

data12_sc_plurality <- mc_hclust(data12_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data12_sca_plurality <- mc_hclust(data12_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data12_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data12_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data12_sc_tapproval <- mc_hclust(data12_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data12_sca_tapproval <- mc_hclust(data12_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data12_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data12_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data12_sc_borda <- mc_hclust(data12_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data12_sca_borda <- mc_hclust(data12_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data12_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data12_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data12_single,
     data12_complete,
     data12_average,
     data12_ward,
     data12_ward2,
     data12_mcquitty,
     data12_median,
     data12_centroid,
     # aggregation methods
     data12_sc_plurality,
     data12_sca_plurality,
     data12_sc_tapproval,
     data12_sca_tapproval,
     data12_sc_borda,
     data12_sca_borda,
     file = "experiments/IPMU2022/results/results_data12.RData")

