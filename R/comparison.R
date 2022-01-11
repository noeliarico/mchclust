evaluate_results <- function(data, mchc) {
  if(class(mchc) == "hclust") {
    clusters <- cutree(mchc, length(unique(data$class)))

  } else {
    clusters <- get_clusters(mchc, length(unique(data$class)))
  }
  cm <- table(clusters, data$class)

  cat(paste0("Correct: ", sum(diag(cm)), "/", sum(cm), " - ",  round(sum(diag(cm))*100/sum(cm), 2), "%"))

  res <- fpc::cluster.stats(dist(data[,1:2]),
                       as.numeric(data$class),
                       as.numeric(clusters))

  res <- tibble::tibble(
    total = length(data$class),
    correct  = sum(diag(cm)),
    entropy = res$entropy,
    dunn = res$dunn,
    rand = res$corrected.rand,
    vi = res$vi
  )

  return(res)



}

res_data01 <- list(
  "single" = data01_s,
  "complete" = data01_c,
  "average" = data01_a,
  "sc_plurality" = data01_sc_plurality,
  "sca_plurality" = data01_sca_plurality,
  "sc_tapproval" = data01_sc_tapproval,
  "sca_tapproval" = data01_sca_tapproval,
  "sc_borda" = data01_sc_borda,
  "sca_borda" = data01_sca_borda
)
get_results_table <- function(data, name, list_mchc, tidy = FALSE) {
  res <- lapply(list_mchc, function(x) evaluate_results(data, x)) %>%
    map_dfr(~ .x, .id = "method") %>%
    mutate(data = name)
  if (tidy) {
    res <- res %>%
      mutate(method = as.factor(method),
             data = as.factor(data)) %>%
      pivot_longer(-c("method", "data"), names_to = "metric") %>%
      mutate(metric = as.factor(metric))
  }
  res
}

get_results_table_hclust <- function(data, name, list_mchc, tidy = FALSE) {
  res <- lapply(res_data01, function(x) evaluate_results(data, x)) %>%
    map_dfr(~ .x, .id = "method") %>%
    mutate(data = name)
  if (tidy) {
    res <- res %>%
      mutate(method = as.factor(method),
             data = as.factor(data)) %>%
      pivot_longer(-c("method", "data"), names_to = "metric") %>%
      mutate(metric = as.factor(metric))
  }
  res
}



plot_ranking <- function(res, metric_filter) {
  res <- res %>%
    filter(metric == metric_filter) %>%
    group_by(data) %>%
    mutate(ranking = rank(value),
           ranking = as.factor(ranking)) %>%
    ungroup()
  min_hclust <- res %>%
    filter(metric == metric_filter) %>%
    filter(!str_detect(method, '_')) %>%
    summarise(min = min(value)) %>%
    pull(min)
  max_hclust <- res %>%
    filter(metric == metric_filter) %>%
    filter(!str_detect(method, '_')) %>%
    summarise(max = max(value)) %>%
    pull(max)
  colfunc <- colorRampPalette(c("#BAECF8", "#03303B"))
  colors <- colfunc(10)
  ggplot(res, aes(method, value, fill = ranking)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    geom_hline(yintercept = min_hclust) +
    geom_hline(yintercept = max_hclust) +
    labs(x = "Linkage method", y = metric_filter,
         fill = "Ranking of\nbest methods") +
    tema
}


evaluate_results(data01b, data01_sc_plurality)
evaluate_results(data01_sca_plurality)
evaluate_results(data01_sc_tapproval)
evaluate_results(data01_sca_tapproval)
evaluate_results(data01_sc_borda)
evaluate_results(data01_sca_borda)

tema <- theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5))

one_mchclust_vs_many_hclust <- function(res, metric) {
  ggplot(res, aes(x = method, y = metric)) +
    geom_bar(
      stat = "identity", position = position_stack(),
      color = "white", fill = "lightblue"
    ) +
    coord_flip()
}

