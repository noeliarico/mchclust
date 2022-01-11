# Number of clusters
nc <- length(unique(data01))
plot_hclust_comparison(data01, nc, mode = "sca")

# Create a subset
set.seed(3)
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

data01_s <- hclust(dist(data01_training), method = "single")
data01_c <- hclust(dist(data01_training), method = "complete")
data01_a <- hclust(dist(data01_training), method = "average")

######## PLURALITY #############################################################

data01_sc_plurality <- mc_hclust(data01_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "plurality",
                             verbose = F)

data01_sca_plurality <- mc_hclust(data01_training,
                                 linkage_methods = c("single", "complete", "average"),
                                 aggregation_method = "plurality",
                                 verbose = F)

plot_mchclust_tiles(data01_sc_plurality, 10) + ggtitle("SC")+ plot_mchclust_tiles(data01_sca_plurality, 10) + ggtitle("SCA")

######## TAPPROVAL #############################################################

data01_sc_tapproval <- mc_hclust(data01_training,
                                 linkage_methods = c("single", "complete"),
                                 aggregation_method = nc,
                                 verbose = F)

data01_sca_tapproval <- mc_hclust(data01_training,
                                  linkage_methods = c("single", "complete", "average"),
                                  aggregation_method = nc,
                                  verbose = F)

plot_mchclust_tiles(data01_sc_tapproval, 10) + ggtitle("SC")+ plot_mchclust_tiles(data01_sca_tapproval, 10) + ggtitle("SCA")

######## BORDA #################################################################

data01_sc_borda <- mc_hclust(data01_training,
                             linkage_methods = c("single", "complete"),
                             aggregation_method = "borda",
                             verbose = F)

data01_sca_borda <- mc_hclust(data01_training,
                              linkage_methods = c("single", "complete", "average"),
                              aggregation_method = "borda",
                              verbose = F)

################################################################################

# Save results

save(data01_sc_plurality, data01_sca_plurality,
     data01_sc_tapproval, data01_sca_tapproval,
     data01_sc_borda, data01_sca_borda,
     file = "experiments/results/results_data01.RData")
