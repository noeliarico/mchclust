# Number of clusters
nc <- length(unique(data02$class))
plot_hclust_comparison(data02, nc, mode = "sca")

# Create a subset
set.seed(2) # changes a lot depending on the seed
data02b <- caret::createDataPartition(
  data02$class,
  p = .15,
  list = F
)
data02b <- data02[data02b,]
data02_training <- data02b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data02b, nc, mode = "sca")
pcsca

data02_single <- hclust(dist(data02_training), method = "single")
data02_complete <- hclust(dist(data02_training), method = "complete")
data02_average <- hclust(dist(data02_training), method = "average")
data02_ward <- hclust(dist(data02_training), method = "ward.D")
data02_ward2 <- hclust(dist(data02_training), method = "ward.D2")
data02_mcquitty <- hclust(dist(data02_training), method = "mcquitty")
data02_median <- hclust(dist(data02_training), method = "median")
data02_centroid <- hclust(dist(data02_training), method = "centroid")

######## PLURALITY #############################################################

data02_sc_plurality <- mc_hclust(data02_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data02_sca_plurality <- mc_hclust(data02_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data02_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data02_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data02_sc_tapproval <- mc_hclust(data02_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data02_sca_tapproval <- mc_hclust(data02_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data02_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data02_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data02_sc_borda <- mc_hclust(data02_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data02_sca_borda <- mc_hclust(data02_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data02_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data02_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data02_single,
     data02_complete,
     data02_average,
     data02_ward,
     data02_ward2,
     data02_mcquitty,
     data02_median,
     data02_centroid,
     # aggregation methods
     data02_sc_plurality,
     data02_sca_plurality,
     data02_sc_tapproval,
     data02_sca_tapproval,
     data02_sc_borda,
     data02_sca_borda,
     file = "experiments/IPMU2022/results/results_data02.RData")

