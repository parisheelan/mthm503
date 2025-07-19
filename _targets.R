library(targets)
library(tidyr)
library(ggplot2)
library(factoextra)
library(dbscan)
library(DBI)
library(RPostgres)
library(skimr)

tar_option_set(packages = c("tidyr", "ggplot2", "factoextra", "dbscan", "DBI", "RPostgres", "skimr"))

source("R/functions.R")  

list(
  tar_target(
    olive_oil_data,
    read_olive_oil_data()
  ),
  tar_target(
    eda_hist_plot,
    eda_histograms(olive_oil_data)
  ),
  tar_target(
    scaled_data,
    scale_numeric_data(olive_oil_data)
  ),
  tar_target(
    pca_obj,
    perform_pca(scaled_data$scaled_data)
  ),
  tar_target(
    pca_variance_plot,
    plot_pca_variance(pca_obj)
  ),
  tar_target(
    pca_scores,
    get_pca_scores(pca_obj, n = 2)
  ),
  tar_target(
    kmeans_res,
    kmeans_clustering(pca_scores, centers = 3)
  ),
  tar_target(
    kmeans_cluster_plot,
    plot_clusters(kmeans_res$clustered_data, cluster_col = "cluster",
                  title = "K-Means Clustering on PCA-Reduced Olive Oil Data")
  ),
  tar_target(
    dbscan_res,
    dbscan_clustering(kmeans_res$clustered_data[, c("PC1", "PC2")], eps = 0.3, minPts = 5)
  ),
  tar_target(
    dbscan_cluster_plot,
    plot_clusters(dbscan_res$clustered_data, cluster_col = "dbscan",
                  title = "DBSCAN Clustering on PCA-Reduced Olive Oil Data")
  )
)
