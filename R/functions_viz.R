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
  mchc <- mchc[[1]]
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
plot_hclust_tiles <- function(hc, log = F) {

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
  if (log) {
    return(plot_log)
  }
  else {
    colnames(plot_log) <- 1:ncol(plot_log)
    plot_log <- plot_log %>%
      tibble::as_tibble() %>%
      dplyr::mutate(level = factor(1:nrow(plot_log))) %>%
      tidyr::pivot_longer(-level, names_to = "object") %>%
      dplyr::mutate(object = as.factor(as.numeric(object)),
                    cluster = as.factor(value),
                    value = NULL)
    ggplot(plot_log,
           aes(object, level, fill = cluster)) +
      geom_tile(color = "black") +
      # xlim(levels(mchc$object)) +
      ylim(rev(levels(plot_log$level))) +
      guides(fill = guide_legend(title="Cluster")) +
      xlab("Object") + ylab("Level") +
      theme_minimal() + theme(legend.position = "none")
  }
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
plot_hclust_comparison <- function(data, k, mode = "sc") {

  tema <- theme_minimal() +
    theme(text = element_text(family = "Times", size = 12),
          plot.margin = unit(c(.5,.5,.5,.5), "cm"),
          axis.title.x = element_text(vjust=-5),
          axis.title.y = element_text(vjust=5),
          legend.position = "none")

  d <- dist(data[,1:2])
  hcs <- hclust(d, method = "single")
  clusterss <- cutree(hcs, k)
  hcc <- hclust(d, method = "complete")
  clustersc <- cutree(hcc, k)
  hca <- hclust(d, method = "average")
  clustersa <- cutree(hca, k)
  ps <- ggplot(data, aes(V1, V2)) +
    geom_point(aes(color = factor(clusterss))) +
    ggtitle("Single") +
    tema
  pc <- ggplot(data, aes(V1, V2)) +
    geom_point(aes(color = factor(clustersc))) +
    ggtitle("Complete") +
    tema
  pa <- ggplot(data, aes(V1, V2)) +
    geom_point(aes(color = factor(clustersa))) +
    ggtitle("Average") +
    tema

  po <- ggplot(data, aes(V1, V2, color = class)) +
    geom_point() +
    ggtitle("Real") +
    theme_bw() +
    theme(text = element_text(family = "Times", size = 12),
          plot.margin = unit(c(1,1,1,1), "cm"),
          axis.title.x = element_text(vjust=-5),
          axis.title.y = element_text(vjust=5),
          legend.position = "none")

  if(mode == "sc") {
    po + ps + pc + patchwork::plot_layout(guides = "collect")
  }
  else if(mode == "sca") {
    po + ps + pc + pa + patchwork::plot_layout(guides = "collect")
  }

}

#' @export
plot_mchclust <- function(mchc, data, k) {
  ggplot(data, aes(V1, V2)) +
    geom_point(aes(color = get_clusters(mchc, k))) +
    theme(legend.position = "none")
}

#' @export
plot_mchclust_comparison <- function(mchc, data, k, mode = "sca") {
  plot_hclust <- plot_hclust_comparison(data, k, mode)
  ggplot(data, aes(V1, V2)) +
    geom_point(aes(color = get_clusters(mchc, k))) +
    theme(legend.position = "none") +
    plot_hclust
}
