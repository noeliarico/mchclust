library(caret)

plot_hclust_comparison(data02,4)

# Subset
set.seed(3)
data02b <- caret::createDataPartition(
  data02$class,
  p = .2,
  list = F
)
data02b <- data02[data02b,]
data02b_training <- data02b[,1:2]
# Check new
plot_hclust_comparison(data02b, 4, mode = "sca")

data02_s <- hclust(dist(data02b_training), method = "single")
data02_c <- hclust(dist(data02b_training), method = "complete")
data02_a <- hclust(dist(data02b_training), method = "average")


data02_sc <- mc_hclust(data02b_training,
                       linkage_methods = c("single", "complete"),
                       verbose = F)

data02_sca <- mc_hclust(data02b_training,
                       linkage_methods = c("single", "complete", "average"),
                       verbose = F)


plot_mchclust_tiles(hcsc_data02[[1]])
clusters <- as.factor(data02_sca[[1]][4,] )
ggplot(data02b, aes(V1, V2)) +
  geom_point(aes(color = clusters))
