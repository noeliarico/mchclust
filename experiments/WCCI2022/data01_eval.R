# Visualize results

hclust_plots <- plot_hclust_comparison(data01b, nc, mode = "sca")
mchclust_plots <- (plot_mchclust(data01_sc_plurality, data01b, nc) + ggtitle("SC plurality")) +
  (plot_mchclust(data01_sc_tapproval, data01b, nc) + ggtitle("SC tapproval")) +
  (plot_mchclust(data01_sc_borda, data01b, nc) + ggtitle("SC borda")) +
  (plot_mchclust(data01_sca_plurality, data01b, nc) + ggtitle("SCA plurality")) +
  (plot_mchclust(data01_sca_tapproval, data01b, nc) + ggtitle("SCA tapproval")) +
  (plot_mchclust(data01_sca_borda, data01b, nc) + ggtitle("SCA borda"))
(mchclust_plots) | (hclust_plots)

