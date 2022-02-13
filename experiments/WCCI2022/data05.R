# Number of clusters
nc <- length(unique(data05$class))
plot_hclust_comparison(data05, nc, mode = "sca")

# Create a subset
set.seed(1) # changes a lot depending on the seed
data05b <- caret::createDataPartition(
  data05$class,
  p = .3,
  list = F
)
data05b <- data05[data05b,]
data05_training <- data05b[,1:2]

# Check the new visualization
pcsca <- plot_hclust_comparison(data05b, nc, mode = "sca")
pcsca

data05_single <- hclust(dist(data05_training), method = "single")
data05_complete <- hclust(dist(data05_training), method = "complete")
data05_average <- hclust(dist(data05_training), method = "average")
data05_ward <- hclust(dist(data05_training), method = "ward.D")
data05_ward2 <- hclust(dist(data05_training), method = "ward.D2")
data05_mcquitty <- hclust(dist(data05_training), method = "mcquitty")
data05_median <- hclust(dist(data05_training), method = "median")
data05_centroid <- hclust(dist(data05_training), method = "centroid")

######## PLURALITY #############################################################

data05_sc_plurality <- mc_hclust(data05_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = "plurality",
                                 verbose = F)

data05_sca_plurality <- mc_hclust(data05_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = "plurality",
                                  verbose = F)

# plot_mchclust_tiles(data05_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data05_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data05_sc_tapproval <- mc_hclust(data05_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data05_sca_tapproval <- mc_hclust(data05_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

# plot_mchclust_tiles(data05_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data05_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data05_sc_borda <- mc_hclust(data05_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data05_sca_borda <- mc_hclust(data05_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

plot_mchclust_tiles(data05_sc_borda, 10) + ggtitle("SC")+ plot_mchclust_tiles(data05_sca_borda, 10) + ggtitle("SCA")

################################################################################

# Save results

save(data05_single,
     data05_complete,
     data05_average,
     data05_ward,
     data05_ward2,
     data05_mcquitty,
     data05_median,
     data05_centroid,
     # aggregation methods
     data05_sc_plurality,
     data05_sca_plurality,
     data05_sc_tapproval,
     data05_sca_tapproval,
     data05_sc_borda,
     data05_sca_borda,
     file = "experiments/IPMU2022/results/results_data05.RData")

