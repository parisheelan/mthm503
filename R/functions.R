library(tidyr)
library(ggplot2)
library(factoextra)
library(dbscan)
library(DBI)
library(RPostgres)
library(skimr)

# Connect and read data
read_olive_oil_data <- function(env_path = ".Renviron.R", table_name = "olive_oil") {
  readRenviron(env_path)
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PGRDATABASE"),
    host = Sys.getenv("PGRHOST"),
    user = Sys.getenv("PGRUSER"),
    password = Sys.getenv("PGRPASSWORD"),
    port = Sys.getenv("PGRPORT")
  )
  on.exit(DBI::dbDisconnect(conn))
  
  olive_oil <- DBI::dbReadTable(conn, table_name)
  return(olive_oil)
}

# Exploratory Data Analysis (returns long-format for plotting)
eda_histograms <- function(olive_oil) {
  numeric_cols <- names(olive_oil)[sapply(olive_oil, is.numeric)]
  olive_long <- pivot_longer(
    olive_oil,
    cols = all_of(numeric_cols),
    names_to = "variable",
    values_to = "value"
  )
  p <- ggplot(olive_long, aes(x = value)) +
    geom_histogram(binwidth = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ variable, scales = "free_x") +
    theme_minimal()
  return(p)
}

# Scale numeric data
scale_numeric_data <- function(olive_oil) {
  numeric_cols <- names(olive_oil)[sapply(olive_oil, is.numeric)]
  olive_scaled <- scale(olive_oil[, numeric_cols])
  return(list(scaled_data = olive_scaled, numeric_cols = numeric_cols))
}

# Perform PCA
perform_pca <- function(scaled_data) {
  pca <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
  return(pca)
}

# Visualize PCA explained variance (optional side effect)
plot_pca_variance <- function(pca) {
  p <- fviz_eig(pca)
  return(p)
}

# Get PCA scores for first n components
get_pca_scores <- function(pca, n = 2) {
  pca_df <- as.data.frame(pca$x[, 1:n])
  colnames(pca_df) <- paste0("PC", 1:n)
  return(pca_df)
}

# K-Means clustering on PCA scores
kmeans_clustering <- function(pca_df, centers = 3, nstart = 25, seed = 123) {
  set.seed(seed)
  km_res <- kmeans(pca_df, centers = centers, nstart = nstart)
  pca_df$cluster <- as.factor(km_res$cluster)
  return(list(clustered_data = pca_df, kmeans_result = km_res))
}

# DBSCAN clustering on PCA scores
dbscan_clustering <- function(pca_df, eps = 0.3, minPts = 5) {
  dbscan_res <- dbscan(pca_df[, 1:2], eps = eps, minPts = minPts)
  pca_df$dbscan <- as.factor(ifelse(dbscan_res$cluster == 0, "Noise", dbscan_res$cluster))
  return(list(clustered_data = pca_df, dbscan_result = dbscan_res))
}

# Plot clustering results (generic)
plot_clusters <- function(df, x = "PC1", y = "PC2", cluster_col = "cluster", title = "") {
  p <- ggplot(df, aes_string(x = x, y = y, color = cluster_col)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(title = title) +
    theme_minimal()
  return(p)
}
