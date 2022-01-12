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
