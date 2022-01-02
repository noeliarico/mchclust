#' #' Multicriteria hirarchical clustering
#' #'
#' #' @param data
#' #' @param methods
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' mc_hclust <- function(data,
#'                       linkage_methods,
#'                      #  aggregatino_methods = NULL,
#'                       verbose = F) {
#'
#'   # Init empty tables for logging
#'   # ranking_log <- tibble::tribble(~cFrom, ~cTo, ~ranking, ~level)
#'   # level_log <- tibble::tribble(~cFrom, ~cTo, ~distance, ~level, ~method)
#'   plot_log <- tibble::tribble(~object, ~level, ~cluster)
#'
#'   # Compute the distance matrix and create a tidy table.
#'   # This tidy table has a format
#'   # cFrom | cTo | distance
#'   # where each pair of clusters is given in a row together with the distance
#'   # between them
#'   dt <- init_distance_table(data %>% dplyr::select(-cluster))
#'   plot_log <- plot_log %>%
#'     dplyr::bind_rows(tibble::tibble(object = 1:nrow(data),
#'                                     level = nrow(data),
#'                                     cluster = 1:nrow(data)))
#'
#'   if(verbose) {
#'     cat("\n-----------------------------------------\n")
#'     cat(paste("-- LEVEL", nrow(data), "------------------------------"))
#'     cat("\n-----------------------------------------\n\n")
#'     cat("Distance between the clusters:\n")
#'     print(dt)
#'   }
#'
#'   # In the first iter, all the rankings are the same, therefore there's no need
#'   # to compute this value
#'   ## Get the closest objects
#'   to_merge <- get_first_cluster(dt, verbose)
#'   ## And create the first cluster
#'   dt <- merge_clusters(dt, to_merge[1], to_merge[2])
#'   plot_log <- update_plot_log(plot_log, to_merge[1], to_merge[2])
#'
#'   if(verbose) {
#'     cat("\nNew distance table:\n")
#'     print(dt)
#'   }
#'
#'   # For the next level results must be aggregated
#'   for(l in (nrow(data)-1):3) {
#'     if(verbose) {
#'       cat("\n-----------------------------------------\n")
#'       cat(paste("-- LEVEL", l, "------------------------------"))
#'       cat("\n-----------------------------------------\n")
#'     }
#'     # Get the ranking for each method and aggregate the results
#'     for (m in linkage_methods) {
#'       r <- get_ranking(dt, m) %>%
#'         dplyr::mutate(cluster = paste0("c",cFrom,"_",cTo),
#'                level = l,
#'                method = m)
#'       level_log <- level_log %>%
#'         dplyr::bind_rows(r)
#'     }
#'
#'     distances_for_profile <- level_log %>%
#'       dplyr::filter(level == l) %>%
#'       dplyr::select(method, cluster, distance) %>%
#'       tidyr::pivot_wider(method, names_from = cluster, values_from = distance)
#'     if(verbose) {
#'       cat("\nDistance between each pair of clusters according to the methods:\n")
#'       print(distances_for_profile)
#'     }
#'
#'     # Get the profile of rankings
#'     por <- consensus::as_por(distances_for_profile %>% dplyr::select(-method))
#'     if(verbose) {
#'       cat("\nProfile of rankings:\n")
#'       print(por)
#'     }
#'
#'     # Apply borda count
#'     borda_ranking <- consensus::borda(por)
#'     if(verbose) {
#'       cat("\nBorda ranking:\n")
#'       print(borda_ranking)
#'     }
#'     # output for building dendrogram
#'     # ranking_log <- update_borda_log(ranking_log, borda_ranking, l)
#'     # Get the name of the winner
#'     winner <- names(borda_ranking)[which(borda_ranking == 1)] # in format cNum_Num
#'     # Get the clusters to merge
#'     to_merge <- as.numeric(stringr::str_split(stringr::str_remove(winner, "c"), "_", simplify = T))
#'     if(verbose) {
#'       cat("-> The objects", to_merge[1], "and", to_merge[2], "create a new cluster\n")
#'     }
#'
#'     dt <- merge_clusters(dt, to_merge[1], to_merge[2])
#'     plot_log <- update_plot_log(plot_log, to_merge[1], to_merge[2])
#'     if(verbose) {
#'       cat("\nNew distance table:\n")
#'       print(dt)
#'     }
#'   }
#'
#'   # Level 2
#'   if(verbose) {
#'     cat("\n-----------------------------------------\n")
#'     cat(paste("-- LEVEL", 2, "------------------------------"))
#'     cat("\n-----------------------------------------\n")
#'   }
#'
#'   # Root
#'
#'   if(verbose) cat("\n-----------------------------------------\n\n")
#'   print("plot_log")
#'   print(plot_log)
#'   return(list(level_log, plot_log))
#' }
#'
#'
#'
