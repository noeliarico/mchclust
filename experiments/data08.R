

library(caret)

plot_hclust_comparison(data08,3, mode = "sca")

# Subset
set.seed(3)
data08b <- caret::createDataPartition(
  data08$class,
  p = .16,
  list = F
)
data08b <- data08[data08b,]
data08b_training <- data08b[,1:2]

# Check new
pcsca <- plot_hclust_comparison(data08b, 3, mode = "sca")

data08_s <- hclust(dist(data08b_training), method = "single")
data08_c <- hclust(dist(data08b_training), method = "complete")
data08_a <- hclust(dist(data08b_training), method = "average")

data08_sc_borda <- mc_hclust(data08b_training,
                       linkage_methods = c("single", "complete"),
                       verbose = F)

clusters <- as.factor(data08_sc_borda[[1]][3,] )
ggplot(data08b, aes(V1, V2)) +
  geom_point(aes(color = clusters)) +
  theme(legend.position = "none") +
  pcsca

data08_sca_borda <- mc_hclust(data08b_training,
                        linkage_methods = c("single", "complete", "average"),
                        verbose = F)

clusters <- as.factor(data08_sca_borda[[1]][3,] )
ggplot(data08b, aes(V1, V2)) +
  geom_point(aes(color = clusters)) +
  theme(legend.position = "none") +
  pcsca
