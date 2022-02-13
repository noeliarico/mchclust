seed <- 1 # 365  299# para 9 con int 285, 116, 94, 43
n <- 8
data <- tibble::tibble(x = sample(1:10, n, T),
                       y = sample(1:10, n, T))

clustering_sca <- mc_hclust(data[,1:2],
                            linkage_methods = c("single", "complete", "average"),
                            aggregation_method = "plurality",
                            verbose = F)

all_equal <- T
# para 7 vale 41 pero se solapan dos puntos
# para 8 87
while(seed < 600 && (any(clustering_sca[[2]]) || all_equal)) {
  seed <- seed+1
  print(seed)
  set.seed(seed)

  # con int
  # data <- tibble::tibble(x = sample(1:10, n, T),
  #                        y = sample(1:10, n, T))

  data <- tibble::tibble(x = rnorm(n),
                         y = rnorm(n))


  set.seed(380)
  data <- tibble::tibble(x = as.double(sample(1:10, n, T)),
                         y = as.double(sample(1:10, n, T)))
  data[8,1] <- 0.5
  clustering_sca <- mc_hclust(data,
                             linkage_methods = c("single", "complete", "average"),
                             aggregation_method = "plurality",
                             verbose = T)
  clustering_s <- mc_hclust(data,
                            linkage_methods = c("single"),
                            verbose = T)
  clustering_c <- mc_hclust(data,
                            linkage_methods = c("complete"),
                            verbose = T)
  clustering_a <- mc_hclust(data,
                            linkage_methods = c("average"),
                            verbose = T)
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
#library(patchwork)
plot_s + plot_c + plot_a + plot_sca + plot_layout(guides = "collect")
ggsave("middle2.png", width = 8, height = 7, units = "cm", dpi = 1000)

data$letter <- paste0("x[", 1:8, "]")#letters[1:8]
#ggplot(data, aes(x, y)) + geom_point() + geom_label(aes(label = letter))

t <- ggplot(data, aes(x, y)) + geom_point(size = 1) +
  geom_text(aes(label = letter), nudge_x = c(rep(0.2, 7), -0.2), nudge_y = 0.5,
            family = "Times New Roman", parse = TRUE, check_overlap = TRUE) +
  lims(x = c(-1,11), y = c(-1,11)) +
  labs(x = parse(text="V[1]"), y = parse(text="V[2]")) +
  theme_linedraw() + tema + theme(axis.title.x = element_text(size = 8),
                                  axis.title.y = element_text(size = 8))

(t |
  (plot_s | plot_c) / (plot_a | plot_sca)) + plot_layout(guides = "collect", widths = c(1,1.2))

t
ggsave("ties_data.png", width = 8, height = 8, units = "cm", dpi = 1000)
