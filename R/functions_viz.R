#' Plot clustering tiles
#'
#' For objects created with the `mc_hclust` function
#'
#' @param mchc
#' @param objects
#'
#' @return
#' @export
#'
#' @examples
plot_mchclust_tiles <- function(mchc, filter = 0) {
  colnames(mchc) <- 1:ncol(mchc)

  if (filter != 0) {
    mchc <- mchc[1:filter,]
  }
  mchc <- mchc %>%
    tibble::as_tibble() %>%
    dplyr::mutate(level = factor(1:nrow(mchc))) %>%
    tidyr::pivot_longer(-level, names_to = "object") %>%
    dplyr::mutate(object = as.factor(as.numeric(object)),
                  cluster = as.factor(value),
                  value = NULL)
  ggplot(mchc,
         aes(object, level, fill = cluster)) +
    geom_tile(color = "black") +
   # xlim(levels(mchc$object)) +
    ylim(rev(levels(mchc$level))) +
    guides(fill = guide_legend(title="Cluster")) +
    xlab("Object") + ylab("Level") +
    theme_minimal() + theme(legend.position = "none")
}

#' Plot clustering tiles
#'
#' For objects created with the `hclust` function
#'
#' @export
plot_hclust_tiles <- function(hc) {

  n <- nrow(hc$merge)+1
  merge <- vector(mode="numeric", n-1)
  plot_log <- matrix(0, nrow = n, ncol = n, byrow = T)
  print(plot_log)
  plot_log[n, ] <- 1:n
  for (i in 1:(n-1)) {
    l <- n-i
    plot_log[l, ] <- plot_log[l+1, ]
    objs <- hc$merge[i,]
    print(objs)
    if (objs[1] < 0) { # First is an object
      if (objs[2] < 0) { # Merge two objects
        merge[i] <- (-1*objs[1])
        plot_log[l, (-1*objs[2])] <- (-1*objs[1])
      } else { # Merge object and cluster
        use <- sort(c((-1*objs[1]),merge[objs[2]]))
        merge[i] <- use[1]
        update <- plot_log[l,] == use[2]
        plot_log[l, update] <- use[1]
      }
    } else { # First is a cluster
      use <- sort(c(merge[objs[1]],merge[objs[2]]))
      merge[i] <- use[1]
      update <- plot_log[l,] == use[2]
      plot_log[l, update] <- use[1]
    }
  }
  return(plot_log)
}

#' Comparison
#'
#' @param data Dataset available in the package with V1, V2, class
#' @param k Number of real clusters
#'
#' @return
#' @export
#'
#' @examples
plot_hclust_comparison <- function(data, k) {
  d <- dist(data[,1:2])
  hcs <- hclust(d, method = "single")
  clusterss <- cutree(hcs, k)
  hcc <- hclust(d, method = "complete")
  clustersc <- cutree(hcc, k)
  po <- ggplot(data, aes(V1, V2, color = class)) +
    geom_point() +
    ggtitle("Real")
  pos <- ggplot(data, aes(V1, V2)) +
    geom_point(aes(color = factor(clusterss))) +
    ggtitle("Single")
  poc <- ggplot(data, aes(V1, V2)) +
    geom_point(aes(color = factor(clustersc))) +
    ggtitle("Complete")
  po + pos + poc + patchwork::plot_layout(guides = "collect")
}
