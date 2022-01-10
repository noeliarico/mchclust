library(caret)

plot_hclust_comparison(data04,4, mode = "sca")

# Subset
set.seed(3)
data04b <- caret::createDataPartition(
  data04$class,
  p = .2,
  list = F
)
data04b <- data04[data04b,]
data04b_training <- data04b[,1:2]
# Check new
plot_hclust_comparison(data04b, 4, mode = "sca")

data04_s <- hclust(dist(data04b_training), method = "single")
data04_c <- hclust(dist(data04b_training), method = "complete")
data04_a <- hclust(dist(data04b_training), method = "average")

data04_sc <- mc_hclust(data04b_training,
                       linkage_methods = c("single", "complete"),
                       verbose = F)

data04_sca <- mc_hclust(data04b_training,
                        linkage_methods = c("single", "complete", "average"),
                        verbose = F)

clusters <- as.factor(data04_sca[[1]][4,] )
ggplot(data04b, aes(V1, V2)) +
  geom_point(aes(color = clusters))

clusters <- as.factor(data04_sc[[1]][4,] )
ggplot(data04b, aes(V1, V2)) +
  geom_point(aes(color = clusters))

Sys.setenv(SPOTIFY_CLIENT_ID = 'ccb5d129b25e4fd0b0c4538a33be5a80')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '7cea48d756804fb8b1b2a6b4622694e4')

access_token <- get_spotify_access_token()

# plot_hclust_comparison(data06,2, mode = "sca")
