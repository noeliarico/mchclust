# seed ok: 34, 36, 46, 48, 49, 53, 67, 88, 112, 118, 120, 133, 163
# me gustan: 48, 164, 180, el que más 188


# BUSCAR #######################################################################

seed <- 1
set.seed(seed)
data <- tibble::tibble(x = sample(1:10, 6, T),
                       y = sample(1:10, 6, T))

clustering_sc <- mc_hclust(data[,1:2],
                            linkage_methods = c("single", "complete"),
                            aggregation_method = "borda",
                            verbose = F)

all_equal <- T
while(seed < 1000 && (any(clustering_sc[[2]]) || all_equal)) {
  seed <- seed+1
  print(seed)
  set.seed(seed)
  data <- tibble::tibble(x = sample(1:10, 6, T),
                         y = sample(1:10, 6, T))
  clustering_sc <- mc_hclust(data,
                              linkage_methods = c("single", "complete"),
                              aggregation_method = "borda",
                              verbose = F)
  clustering_s <- mc_hclust(data,
                            linkage_methods = c("single"),
                            verbose = F)
  clustering_c <- mc_hclust(data,
                            linkage_methods = c("complete"),
                            verbose = F)
  all_equal <-
    all(clustering_sc[[1]] == clustering_s[[1]]) ||
    all(clustering_sc[[1]] == clustering_c[[1]]) ||
    all(clustering_s[[1]] == clustering_c[[1]])

  print(any(clustering_sc[[2]]))
  print(all_equal)
}
print(paste("Encontrada:", seed))

plot_sc <- plot_mchclust_tiles(clustering_sc) + ggtitle("Borda")
plot_s <- plot_mchclust_tiles(clustering_s) + ggtitle("Single")
plot_c <- plot_mchclust_tiles(clustering_c) + ggtitle("Complete")
plot_s + plot_c + plot_sc + plot_layout(guides = "collect")

data$letter <- letters[1:6]
ggplot(data, aes(x, y)) + geom_point() + geom_label(aes(label = letter))

ggplot(data, aes(x, y)) + geom_point() + geom_label(aes(label = letter)) +
  plot_s + plot_c + plot_sc + plot_layout(guides = "collect")

# PROBAR #######################################################################

set.seed(188)
data <- tibble::tibble(x = sample(1:10, 6, T),
                       y = sample(1:10, 6, T))

clustering_sc <- mc_hclust(data,
                           linkage_methods = c("single", "complete"),
                           aggregation_method = "borda",
                           verbose = T)
clustering_s <- mc_hclust(data,
                          linkage_methods = c("single"),
                          verbose = T)
clustering_c <- mc_hclust(data,
                          linkage_methods = c("complete"),
                          verbose = T)

plot_sc <- plot_mchclust_tiles(clustering_sc) + ggtitle("Borda")
plot_s <- plot_mchclust_tiles(clustering_s) + ggtitle("Single")
plot_c <- plot_mchclust_tiles(clustering_c) + ggtitle("Complete")
plot_s + plot_c + plot_sc + plot_layout(guides = "collect")

data$letter <- letters[1:6]

ggplot(data, aes(x, y)) + geom_point() + geom_label(aes(label = letter)) +
  plot_s + plot_c + plot_sc + plot_layout(guides = "collect")

