# LOAD ALL RESULTS #############################################################
numbers <- stringr::str_pad(1:2, 2, pad = "0")
methods <- c("sc", "sca")
aggregation <- c("plurality", "tapproval", "borda")
endings <- apply(expand.grid(numbers, methods, aggregation), 1, paste, collapse = "_")
variables <- paste0("data", endings)
results <- mget(variables)

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

get_results_table(data01b, "data01", res_data01, tidy = T)

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

res_data02 <- list(
  "single" = data02_s,
  "complete" = data02_c,
  "average" = data02_a,
  "sc_plurality" = data02_sc_plurality,
  "sca_plurality" = data02_sca_plurality,
  "sc_tapproval" = data02_sc_tapproval,
  "sca_tapproval" = data02_sca_tapproval,
  "sc_borda" = data02_sc_borda,
  "sca_borda" = data02_sca_borda
)

res <- bind_rows(
  get_results_table(data01b, "data01", res_data01, tidy = T),
  get_results_table(data02b, "data02", res_data02, tidy = T))

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

tema <- theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5))

one_mchclust_vs_many_hclust <- function(res, metric_filter, data_filter,
                                        aggregation_filter) {
  res <- res %>%
    filter(metric == metric_filter,
           data == data_filter)
  res <- res %>%
    mutate(better = value >= (res %>%
                                filter(method == aggregation_filter) %>%
                                pull(value)))
  print(res)
  ggplot(res, aes(x = method, y = value)) +
    geom_bar(
      stat = "identity", position = position_stack(),
      color = "white", fill = "lightblue"
    )
    # coord_flip()
}

best_metric_in_all_data <- function(res) {
  res <- res %>%
    group_by(data, metric) %>%
    mutate(ranking = rank(value, ties.method = "first")) %>%
    ungroup() %>%
    filter(ranking == 1) %>%
    group_by(metric, method) %>%
    count() %>%
    ungroup() %>%
    mutate(n = factor(n))
  ggplot(res, aes(method, n)) +
    geom_bar(stat = "identity") +
    facet_grid(metric ~ .) +
    tema
}
