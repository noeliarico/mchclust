#' Init distance table
#'
#' The distance matrix is compute for the `data` given as argument using
#' the `base::dist` function.
#'
#' The aim of this function is to create a table of the format
#' cFrom | cTo | distance
#' that computes in a tidy format for each pair of clusters the distance
#' between them.
#'
#' @param data table with data for computing distance
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(123)
#' # Artificial data for five objects
#' obj <- tibble::tibble(x = sample(1:10,5), y = sample(1:10,5))
#' # Each object is one cluster, pairwise comparison of the clusters given by
#' init_distance_matrix(obj)
#'
init_distance_table <- function(data) {
  rownames(data) <- NULL
  dm <- as.matrix(dist(data)) # create distance matrix
  dm[lower.tri(dm, diag = T)] <- Inf # consider values only once
  dt <- reshape2::melt(dm,
                       varnames = c("cFrom", "cTo")) %>%
    #dplyr::rename(distance = value) %>%
    dplyr::filter(value != Inf) %>%
    dplyr::filter(cFrom != cTo)
  return(dt)
}


#' Get first cluster
#'
#' In the first iteration all the objects represent individual clusters.
#' Therefore all the linkage method would decide the same winner.
#' For this reason is not necessary to create and aggregate the rankings
#' as the row with the closest distance is the one selected.
#'
#' @param distance_table
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
get_first_cluster <- function(distance_table, verbose = F) {
  closest_clusters <- distance_table %>%
    dplyr::slice(which.min(value)) %>%
    dplyr::select(-value)

  if(nrow(closest_clusters) > 1) {
    warning("There's a tie, more than two clusters are the closest")
  }
  if(verbose)  {
    cat(paste("-> Closest clusters are", closest_clusters[1,1],
              "and", closest_clusters[1,2], "\n"))
  }

  # Return vector with the indexes of the two closest clusters
  return(c(closest_clusters[1,1], closest_clusters[1,2]))
}

#' Merge clusters
#'
#' This recieves a distance table and merge the clusters together
#'
#' @param distance_table tidy distance table as the one given by
#' `init_distance_matrix`.
#' @param merge1 The index of the first cluster to merge
#' @param merge2 The index of the second cluster to merge
#'
#' @return The new distance table for the upper level of the dendrogram
#' @export
#'
#' @examples
merge_clusters <- function(distance_table, merge1, merge2) {
  dt <- distance_table %>%
    # Rename the columns of the clusters that have been merged
    dplyr::mutate(cFrom = ifelse(cFrom == merge2, merge1, cFrom),
                  cTo = ifelse(cTo == merge2, merge1, cTo)) %>%
    dplyr::filter(cFrom != cTo)
  new <- t(apply(dt[,1:2], 1, function(x) c(min(x), max(x))))
  dt[,1:2] <- new
  return(dt)
}
# merge_clusters <- function(dm, merge1, merge2) {
#   # Update column names
#   objs_col <- colnames(dm)
#   objs_col[as.numeric(objs_col) == merge2] <- merge1
#   colnames(dm) <- objs_col
#   # Update row names
#   objs_rows <- rownames(dm)
#   objs_rows[as.numeric(objs_rows) == merge2] <- merge1
#   rownames(dm) <- objs_rows
#
#   return(dm)
# }


#' @export
update_plot_log <- function(plot_log, merge1, merge2, level) {
  prev <- plot_log[level, ]
  prev[prev == merge2] <- merge1
  plot_log[level-1, ] <- prev
  return(plot_log)
}

# update_plot_log <- function(plot_log, merge1, merge2) {
#   plot_log_new <- plot_log
#   plot_log_new <- plot_log_new %>%
#     dplyr::mutate(level = level-1,
#            cluster = ifelse(cluster == merge2, merge1, cluster))
#   plot_log <- plot_log %>%
#     dplyr::bind_rows(plot_log_new)
#   return(plot_log)
# }




#' Title
#'
#' @param distance_table
#' @param method
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(distance_table,
                        method = NULL,
                        verbose = F) {
  d <- distance_table %>%
    dplyr::group_by(cFrom, cTo)
  if(method == "single") {
    d <- d %>%
      dplyr::summarise(distance = min(value), .groups = 'drop')
  }
  else if(method == "complete") {
    d <- d %>%
      dplyr::summarise(distance = max(value), .groups = 'drop')
  }
  else {
    stop("Unknown method")
  }

  if(verbose) print(d) # testing purposes

  return(d)
}

#' Update borda log
#'
#' @param borda_log
#' @param borda_ranking
#' @param l
#'
#' @return
#' @export
#'
#' @examples
update_borda_log <- function(borda_log, borda_ranking, l) {
  ranking <- as.numeric(borda_ranking)
  # print(ranking)
  n <- names(borda_ranking)
  # print(n)
  clusters <- sapply(stringr::str_split(stringr::str_remove(n, "c"), "_"), as.numeric)
  # print(clusters)
  borda_log <- borda_log %>%
    dplyr::bind_rows(tibble::tibble(cFrom = clusters[1,],
                                    cTo = clusters[2,],
                                    ranking = as.numeric(borda_ranking),
                                    level = l))
  return(borda_log)
}



x <- tibble::tibble(x = sample(1:20, 10),
                    y = sample(1:20, 10),
                    z = sample(1:20, 10))
