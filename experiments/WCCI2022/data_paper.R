library(tibble)
set.seed(123)

# First example
# data <- tibble::tibble(x = c(.9, 2, 3.75, 4, 3, 5.1),
#                         y = c(3.74, 2, (15-sqrt(110))/5, 2.76, 3.76, 3.76),
#                        name = factor(1:6))

# Another valid example
# data <- tibble::tibble(x = c(.9, 2.5, 3.75, 4, 3, 5.1),
#                        y = c(3.74, 2.5, (15-sqrt(110))/5, 2.76, 3.76, 3.76),
#                        name = factor(1:6))

ggplot(data, aes(x, y)) +
  geom_point() +
  geom_label(aes(label = name))

clustering_sc <- mc_hclust(data[,1:2],
                           linkage_methods = c("single", "complete"),
                           verbose = T)

# Esto da sin empates
# set.seed(10)
# data <- tibble::tibble(x = sample(1:100, 10, T),
#                        y = sample(1:100, 10, T))

# semillas que sirven: 34, 36, 46
seed <- 47
set.seed(seed)
data <- tibble::tibble(x = sample(1:10, 6, T),
                       y = sample(1:10, 6, T))
clustering_sc <- mc_hclust(data[,1:2],
                           linkage_methods = c("single", "complete"),
                           verbose = T)
iguales <- T
while(seed < 1000 && (any(clustering_sc[[2]]) || iguales)) {
  seed <- seed+1
  print(seed)
  set.seed(seed)
  data <- tibble::tibble(x = sample(1:10, 6, T),
                         y = sample(1:10, 6, T))
  clustering_sc <- mc_hclust(data,
                             linkage_methods = c("single", "complete"),
                             verbose = T)
  clustering_s <- mc_hclust(data,
                            linkage_methods = c("single"),
                            verbose = F)
  clustering_c <- mc_hclust(data,
                            linkage_methods = c("complete"),
                            verbose = F)
  iguales <- all(clustering_sc[[1]] == clustering_s[[1]]) ||
             all(clustering_sc[[1]] == clustering_c[[1]])
}
print(paste("Encontrada:", seed))

plot_sc <- plot_mchclust_tiles(clustering_sc) + ggtitle("Borda")
plot_s <- plot_mchclust_tiles(clustering_s) + ggtitle("Single")
plot_c <- plot_mchclust_tiles(clustering_c) + ggtitle("Complete")
plot_s + plot_c + plot_sc + plot_layout(guides = "collect")

data$letter <- letters[1:6]
ggplot(data, aes(x, y)) + geom_point() + geom_label(aes(label = letter))

# Using only single method ----------------------
clustering_s <- mc_hclust(data,
                          linkage_methods = c("single"),
                          verbose = T)
plot_s <- plot_mchclust_tiles(clustering_s) + ggtitle("Single")
# Must give the same results than hclust
# plot_hclust_tiles(hclust(dist(data), method = "single")) + plot_s

# Using only complete method ----------------------
clustering_c <- mc_hclust(data,
                 linkage_methods = c("complete"),
                 verbose = T)

# Must give the same results than hclust
plot_c <- plot_mchclust_tiles(clustering_c) + ggtitle("Complete")
# plot_hclust_tiles(hclust(dist(data), method = "complete")) + plot_c

plot_s + plot_c + plot_sc + plot_layout(guides = "collect")
hc

# Using plurality


clustering_sca <- mc_hclust(data,
                           linkage_methods = c("single", "complete", "average"),
                           aggregation_method = "plurality",
                           verbose = T)

clustering_sca <- mc_hclust(data,
                            linkage_methods = c("single", "complete", "average"),
                            aggregation_method = "2",
                            verbose = T)
