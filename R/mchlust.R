#' Multicriteria hirarchical clustering
#'
#' @param data
#' @param methods
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
mc_hclust <- function(data,
                      linkage_methods,
                      aggregation_method = "borda",
                      verbose = F) {

  # Init empty table for logging: for n objects, matrix of n x n where each row
  # is a level of the dendrogram and each column an object.
  # The element in the ith row and jth is the cluster of each object j in
  # each level i. When two clusters are merged the lower index is kept.
  plot_log <- matrix(0, ncol = nrow(data), nrow = nrow(data))

  # Store those levels where there are ties
  ties <- vector(length = nrow(data))

  # Compute the distance matrix and create a tidy table.
  # This tidy table has a format
  # cFrom | cTo | distance
  # where each pair of clusters is given in a row together with the distance
  # between them
  dt <- init_distance_table(data)
  plot_log[nrow(data), ] <- 1:nrow(data) # the bottom each object is a cluster

  if(verbose) {
    cat("\n-----------------------------------------\n")
    cat(paste("-- LEVEL", nrow(data), "------------------------------"))
    cat("\n-----------------------------------------\n\n")
    cat("Distance between the clusters:\n")
    print(dt)
  }

  # In the first iter, the rankings given by all the linked methods are the
  # same as each cluster only contains one single object
  ## Get the closest objects
  to_merge <- get_first_cluster(dt, verbose)
  ## And create the first cluster
  dt <- merge_clusters(dt, to_merge[1], to_merge[2])
  plot_log <- update_plot_log(plot_log, to_merge[1], to_merge[2], nrow(data))

  if(verbose) {
    cat("\nNew distance table:\n")
    print(dt)
  }

  # For the next level results must be aggregated
  for(l in (nrow(data)-1):3) {
    if(verbose) {
      cat("\n-----------------------------------------\n")
      cat(paste("-- LEVEL", l, "------------------------------"))
      cat("\n-----------------------------------------\n")
    }
    else {
      cat(paste(l, "\n"))
    }

    level_log <- tibble::tribble(~cFrom, ~cTo, ~distance, ~cluster, ~method)
    # Get the ranking for each method and aggregate the results
    for (m in linkage_methods) {
      r <- get_ranking(dt, m) %>%
        dplyr::mutate(cluster = paste0("c",cFrom,"_",cTo),
                      method = m)
      level_log <- level_log %>%
        dplyr::bind_rows(r)
    }

    # Create table where each row is a method, its column a pair of cluster and
    # each value the distance between that pair of clusters
    distances_for_profile <- level_log %>%
      dplyr::select(distance, cluster, method) %>%
      tidyr::pivot_wider(method, names_from = cluster, values_from = distance)

    if(verbose) {
      cat("\nDistance between each pair of clusters according to the methods:\n")
      print(distances_for_profile)
    }

    # Get the profile of rankings
    por <- consensus::as_por(distances_for_profile %>% dplyr::select(-method))
    if(verbose) {
      cat("\nProfile of rankings:\n")
      print(por)
    }

    # Apply aggregation method

    if(aggregation_method == "plurality") {
      winning_ranking <- consensus::plurality(por)
      if(verbose) {
        cat("\nPlurality ranking:\n")
        print(winning_ranking)
      }
    }
    else if(aggregation_method == "borda") {
      winning_ranking <- consensus::borda(por)
      if(verbose) {
        cat("\nBorda ranking:\n")
        print(winning_ranking)
      }
    }
    else if(!is.na(as.numeric(aggregation_method))) {
      t <- as.numeric(aggregation_method)
      winning_ranking <- consensus::tapproval(por, t)
      if(verbose) {
        cat("\nt-approval ranking:\n")
        print(winning_ranking)
      }
    } else {
      stop("Unkown aggregation method")
    }


    # Get the name of the winner in format cNum_Num
    winner <- (names(winning_ranking)[which(winning_ranking == 1)])
    if (length(winner) > 1) {
      ties[l] <- T
    }
    winner <- winner[1]

    # Get the clusters to merge
    to_merge <- as.numeric(stringr::str_split(stringr::str_remove(winner, "c"),
                                              "_", simplify = T))
    if(verbose) {
      cat("-> The objects", to_merge[1], "and",
          to_merge[2], "create a new cluster\n")
    }

    # Update the distance table to reflect the merge
    dt <- merge_clusters(dt, to_merge[1], to_merge[2])
    plot_log <- update_plot_log(plot_log, to_merge[1], to_merge[2], l)
    if(verbose) {
      cat("\nNew distance table:\n")
      print(dt)
    }
  }


  # Level 2: merge two remaining clusters
  to_merge <- c(dt %>% dplyr::pull(cFrom) %>% unique(),
                dt %>% dplyr::pull(cTo) %>% unique())
  plot_log <- update_plot_log(plot_log, to_merge[1], to_merge[2], 2)

  # Root: all ones

  if(verbose) cat("\n-----------------------------------------\n\n")
  return(list(plot_log, ties))
}


#' @export
get_clusters <- function(mchc, k) {
  clusters <- factor(mchc[[1]][k,])
  levels(clusters) <- 1:length(levels(clusters))
  return(clusters)
}

