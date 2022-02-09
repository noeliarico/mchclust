# LOAD ALL RESULTS #############################################################
numbers <- stringr::str_pad(1:2, 2, pad = "0")

get_list_results <- function(id) {
  methods <- c("sc", "sca")
  aggregation <- c("plurality", "tapproval", "borda")
  endings <- apply(expand.grid(id, methods, aggregation), 1, paste, collapse = "_")
  variables <- paste0("data", endings)
  variables <- c(variables, paste0("data", id, "_", c("single", "complete", "average")))
  results <- mget(variables, envir = globalenv())
  results
}

res_data01 <- get_list_results("01")
res_data02 <- get_list_results("02")
res_data04 <- get_list_results("04")
res_data05 <- get_list_results("05")
res_data06 <- get_list_results("06")
res_data07 <- get_list_results("07")
res_data08 <- get_list_results("08")
res_data09 <- get_list_results("09")
res_data10 <- get_list_results("10")
res_data11 <- get_list_results("11")
res_data12 <- get_list_results("12")
res_data13 <- get_list_results("13")
res_data14 <- get_list_results("14")
res_data15 <- get_list_results("15")
res_data16 <- get_list_results("16")

# GET TABLES WITH METRICS USING THE RESULTS ####################################

## Define functions ------------------------------------------------------------

# Get metrics from data
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

# Example for one single mchc method
# evaluate_results(data01b, data01_sc_plurality)

# Get the metrics in table
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

## Obtain the table for the data

# Get them for all data
res <- bind_rows(
  get_results_table(data01b, "data01", res_data01, tidy = T),
 # get_results_table(data02b, "data02", res_data02, tidy = T),
  get_results_table(data04b, "data04", res_data04, tidy = T),
  get_results_table(data05b, "data05", res_data05, tidy = T),
  get_results_table(data06b, "data06", res_data06, tidy = T),
  get_results_table(data07b, "data07", res_data07, tidy = T),
  get_results_table(data08b, "data08", res_data08, tidy = T),
  get_results_table(data09b, "data09", res_data09, tidy = T),
  get_results_table(data10b, "data10", res_data10, tidy = T),
  get_results_table(data11b, "data11", res_data11, tidy = T),
  get_results_table(data12b, "data12", res_data12, tidy = T),
  get_results_table(data13b, "data13", res_data13, tidy = T),
  get_results_table(data14b, "data14", res_data14, tidy = T),
  get_results_table(data15b, "data15", res_data15, tidy = T),
  get_results_table(data16b, "data16", res_data16, tidy = T)) %>%
  mutate(method = str_remove(method, "data.._"))



############### Results

plot_ranking <- function(res, metric_filter, data_filter) {
  res <- res %>%
    filter(metric == metric_filter) %>%
    filter(data == data_filter) %>%
    mutate(ranking = rank(value, ties.method = "min"),
           ranking = as.factor(ranking))
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
  colors <- colfunc(16)
  ggplot(res, aes(method, value, fill = ranking)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    geom_hline(yintercept = min_hclust) +
    geom_hline(yintercept = max_hclust) +
    labs(x = "Linkage method", y = metric_filter,
         fill = "Ranking of\nbest methods") +
    tema
}

plot_ranking(res %>% filter(method %in% c("single",
                                          "complete",
                                          "average",
                                          "sca_plurality",
                                          "sca_tapproval")),
             "correct",
             "data11")

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

r <- rank(c(.4, .6, .5, .2, .1, .5, .2), ties.method = "min")
missing <- rev(which(!(1:length(r) %in% r)))
for(i in missing) {
  r <- ifelse(r > i, r-1, r)
}

