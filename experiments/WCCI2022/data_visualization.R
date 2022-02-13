tema <- theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(size = 11),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        axis.title = element_text(size = 11),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=2))

# The datasets #################################################################

plot_basic <- function(data, name = "") {
  ggplot(data, aes(V1, V2, color = class)) +
    geom_point() +
    tema +
    theme(legend.position = "none") +
    scale_color_manual(values = c(brewer.pal(n = 8, name = "Dark2"), "black")) +
    ggtitle(paste0(name, " - ", "(Objects: ", nrow(data), ", Clusters: ", length(unique(data$class)), ")"))
}

# (plot_basic(data01b, "01") | plot_basic(data02b, "02") | plot_basic(data04b, "03")) /
#   (plot_basic(data05b, "04") | plot_basic(data06b, "05") | plot_basic(data07b, "06")) /
#   (plot_basic(data08b, "07") | plot_basic(data09b, "08") | plot_basic(data10b, "09")) /
#   (plot_basic(data11b, "10") | plot_basic(data12b, "11") | plot_basic(data13b, "12")) /
#   (plot_basic(data15b, "13") | plot_basic(data14b, "14") | plot_basic(data16b, "15"))

(plot_basic(data01b, "01") | plot_basic(data02b, "02") | plot_basic(data04b, "03")) /
  (plot_basic(data07b, "04") | plot_basic(data08b, "05") | plot_basic(data09b, "06")) /
  (plot_basic(data10b, "07") | plot_basic(data11b, "08") | plot_basic(data12b, "09")) /
  (plot_basic(data13b, "10") | plot_basic(data14b, "11") | plot_basic(data15b, "12"))


(plot_basic(data01b, "01") | plot_basic(data02b, "02")) /
  (plot_basic(data04b, "03") | plot_basic(data07b, "04")) /
  (plot_basic(data08b, "05") | plot_basic(data09b, "06")) /
  (plot_basic(data10b, "07") | plot_basic(data11b, "08")) /
  (plot_basic(data12b, "09") | plot_basic(data13b, "10")) /
  (plot_basic(data14b, "11") | plot_basic(data15b, "12"))


# Comparison with different hirarchical clustering methods #####################

plot_hclust_comparison(data01,3, mode = "sca")
plot_hclust_comparison(data02,4, mode = "sca")
#plot_hclust_comparison(data03,4)
plot_hclust_comparison(data04,3, mode = "sca")
plot_hclust_comparison(data05,15)
plot_hclust_comparison(data06,2, mode = "sca")
plot_hclust_comparison(data07,3, mode = "sca")
plot_hclust_comparison(data08,3, mode = "sca")
plot_hclust_comparison(data09,2, mode = "sca")
plot_hclust_comparison(data10,4, mode = "sca")
plot_hclust_comparison(data11,2, mode = "sca")
plot_hclust_comparison(data12,9, mode = "sca")
plot_hclust_comparison(data13,2, mode = "sca")
plot_hclust_comparison(data14,4, mode = "sca")
plot_hclust_comparison(data15,4, mode = "sca")
plot_hclust_comparison(data16,2, mode = "sca")
