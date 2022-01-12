# Number of clusters
nc <- length(unique(data01$class))
plot_hclust_comparison(data01, nc, mode = "sca")

# Create a subset
set.seed(3) # changes a lot depending on the seed
data01b <- caret::createDataPartition(
  data01$class,
  p = .2,
  list = F
)
data01b <- data01[data01b,]
data01_training <- data01b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data01b, nc, mode = "sca")
pcsca

data01_single <- hclust(dist(data01_training), method = "single")
data01_complete <- hclust(dist(data01_training), method = "complete")
data01_average <- hclust(dist(data01_training), method = "average")
data01_ward <- hclust(dist(data01_training), method = "ward.D")
data01_ward2 <- hclust(dist(data01_training), method = "ward.D2")
data01_mcquitty <- hclust(dist(data01_training), method = "mcquitty")
data01_median <- hclust(dist(data01_training), method = "median")
data01_centroid <- hclust(dist(data01_training), method = "centroid")

######## PLURALITY #############################################################

data01_sc_plurality <- mc_hclust(data01_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data01_sca_plurality <- mc_hclust(data01_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data01_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data01_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data01_sc_tapproval <- mc_hclust(data01_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data01_sca_tapproval <- mc_hclust(data01_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data01_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data01_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data01_sc_borda <- mc_hclust(data01_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data01_sca_borda <- mc_hclust(data01_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

# plot_mchclust_tiles(data01_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data01_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data01_single,
     data01_complete,
     data01_average,
     data01_ward,
     data01_ward2,
     data01_mcquitty,
     data01_median,
     data01_centroid,
     # aggregation methods
     data01_sc_plurality,
     data01_sca_plurality,
     data01_sc_tapproval,
     data01_sca_tapproval,
     data01_sc_borda,
     data01_sca_borda,
     file = "experiments/IPMU2022/results/results_data01.RData")

