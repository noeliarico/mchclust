test_that("test single and complete", {
  # datos <- tibble::tibble(x = c(.9, 2, 3.75, 4, 3, 5.1),
  #                 y = c(3.74, 2, (15-sqrt(110))/5, 2.76, 3.76, 3.76),
  #                 cluster = 1:6)

  datos <- tibble::tibble(x = c(.9, 2, 3.75, 4, 3, 5.1),
                          y = c(3.74, 2, (15-sqrt(110))/5, 2.76, 3.76, 3.76))

  set.seed(123)
  datos <- tibble::tibble(x = sample(1:20,15),
                          y = sample(1:20,15))

  datos <- iris[c(1:5,51:55,101:105),1:4] # ok
  #datos <- iris[c(1:5,50:54,100:104),1:4] # ok y mas variado

  set.seed(123)
  datos <- iris[c(sample(1:50,10),
                  sample(51:100,10),
                  sample(101:151,10)), 1:4]

  set.seed(123) # con este salen mal
  datos <- iris[c(sample(1:50,5),
                  sample(51:100,5),
                  sample(101:151,5)), 1:4]


  datos <- iris[,1:4]

  data01b <- caret::createDataPartition(
    data01$class,
    p = .3,
    list = F
  )
  data01b <- data01[data01b,1:2]
  hcsc_data01 <- mc_hclust(data01b,
                  linkage_methods = c("single", "complete"),
                  #linkage_methods = c("single"),
                  #linkage_methods = c("complete"),
                     verbose = F)
  sc <- plot_cluster_tiles(hcsc[[1]]) + ggtitle("Borda")
  hcs <- mc_hclust(datos,
                  linkage_methods = c("single"),
                  #linkage_methods = c("complete"),
                  verbose = F)
  s <- plot_cluster_tiles(hcs[[1]]) + ggtitle("Single")
  hcc <- mc_hclust(datos,
                  linkage_methods = c("complete"),
                  verbose = T)
  c <- plot_cluster_tiles(hcc[[1]]) + ggtitle("Complete")
  s + c + sc + plot_layout(guides = "collect")
  hc


  ggplot(data1, aes(V1, V2)) + geom_point(aes(color = factor(clusters)))

  plot_hclust_comparison(data01b,3)
  data01b_s <- hclust(dist(data01b), method = "single")
  data01b_c <- hclust(dist(data01b), method = "complete")


  plot_hclust_comparison(data02,4)
  data02b <- caret::createDataPartition(
    data02$class,
    p = .2,
    list = F
  )
  data02b <- data02[data02b,]
  data02b_training <- data02b[,1:2]
  hcsc_data02 <- mc_hclust(data02b_training,
                           linkage_methods = c("single", "complete"),
                           #linkage_methods = c("single"),
                           #linkage_methods = c("complete"),
                           verbose = F)

  plot_hclust_comparison(data03,4)
  plot_hclust_comparison(data04,3)
  plot_hclust_comparison(data05,15)
  plot_hclust_comparison(data06,2)
  plot_hclust_comparison(data07,3)
  plot_hclust_comparison(data08,3)
  plot_hclust_comparison(data09,2)
  plot_hclust_comparison(data10,4)
})
