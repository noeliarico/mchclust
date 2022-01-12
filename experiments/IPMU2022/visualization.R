tema <- theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5))


plot_hclust_comparison(data01b,3, mode = "sca")
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
