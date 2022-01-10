plot_hclust_comparison(data01, 3, mode = "sca")

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
pcsca <- plot_hclust_comparison(data01b, 3, mode = "sca")
pcsca

data01_s <- hclust(dist(data01_training), method = "single")
data01_c <- hclust(dist(data01_training), method = "complete")
data01_a <- hclust(dist(data01_training), method = "average")

######## PLURALITY #############################################################

data01_sc_plurality <- mc_hclust(data01_training,
                             linkage_methods = c("single", "complete"),
                             verbose = F)

data01_sca_plurality <- mc_hclust(data01_training,
                                 linkage_methods = c("single", "complete"),
                                 verbose = F)

######## TAPPROVAL #############################################################

data01_sc_tapproval <- mc_hclust(data01_training,
                                 linkage_methods = c("single", "complete"),
                                 verbose = F)

data01_sca_tapproval <- mc_hclust(data01_training,
                                  linkage_methods = c("single", "complete"),
                                  verbose = F)

######## BORDA #################################################################

data01_sc_borda <- mc_hclust(data01_training,
                             linkage_methods = c("single", "complete"),
                             verbose = F)

clusters <- as.factor(data01_sc_borda[[1]][3,] )
ggplot(data01b, aes(V1, V2)) +
  geom_point(aes(color = clusters)) +
  theme(legend.position = "none") +
  pcsca

data01_sca_borda <- mc_hclust(data01_training,
                              linkage_methods = c("single", "complete", "average"),
                              verbose = F)

clusters <- as.factor(data01_sca_borda[[1]][3,] )
ggplot(data01b, aes(V1, V2)) +
  geom_point(aes(color = clusters)) +
  pcsca
