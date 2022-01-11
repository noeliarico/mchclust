seed <- 1
set.seed(seed)
data <- tibble::tibble(x = sample(1:10, 6, T),
                       y = sample(1:10, 6, T))

clustering_sca <- mc_hclust(data[,1:2],
                            linkage_methods = c("single", "complete", "average"),
                            aggregation_method = "plurality",
                            verbose = F)

all_equal <- T
while(seed < 1000 && (any(clustering_sca[[2]]) || all_equal)) {
  seed <- seed+1
  print(seed)
  set.seed(seed)
  data <- tibble::tibble(x = sample(1:10, 6, T),
                         y = sample(1:10, 6, T))
  clustering_sca <- mc_hclust(data,
                             linkage_methods = c("single", "complete", "average"),
                             aggregation_method = "plurality",
                             verbose = F)
  clustering_s <- mc_hclust(data,
                            linkage_methods = c("single"),
                            verbose = F)
  clustering_c <- mc_hclust(data,
                            linkage_methods = c("complete"),
                            verbose = F)
  clustering_a <- mc_hclust(data,
                            linkage_methods = c("average"),
                            verbose = F)
  all_equal <-
    all(clustering_sca[[1]] == clustering_s[[1]]) ||
    all(clustering_sca[[1]] == clustering_c[[1]]) ||
    all(clustering_sca[[1]] == clustering_a[[1]]) ||
    all(clustering_s[[1]] == clustering_c[[1]]) ||
    all(clustering_s[[1]] == clustering_a[[1]]) ||
    all(clustering_c[[1]] == clustering_a[[1]])

  print(any(clustering_sca[[2]]))
  print(all_equal)
}
print(paste("Encontrada:", seed))

plot_sca <- plot_mchclust_tiles(clustering_sca) + ggtitle("Plurality")
plot_s <- plot_mchclust_tiles(clustering_s) + ggtitle("Single")
plot_c <- plot_mchclust_tiles(clustering_c) + ggtitle("Complete")
plot_a <- plot_mchclust_tiles(clustering_a) + ggtitle("Average")
plot_s + plot_c + plot_a + plot_sca + plot_layout(guides = "collect")

data$letter <- letters[1:6]
ggplot(data, aes(x, y)) + geom_point() + geom_label(aes(label = letter))

ggplot(data, aes(x, y)) + geom_point() + geom_label(aes(label = letter)) +
  plot_s + plot_c + plot_a + plot_sc + plot_layout(guides = "collect")
