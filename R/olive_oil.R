install.packages("factoextra")
install.packages("dbscan")

library(tidyr)
library(ggplot2)
library(factoextra)
library(dbscan)

readRenviron(".Renviron.R")
Sys.getenv("PGRUSER")

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("PGRDATABASE"),
  host = Sys.getenv("PGRHOST"),
  user = Sys.getenv("PGRUSER"),
  password = Sys.getenv("PGRPASSWORD"),
  port = Sys.getenv("PGRPORT")
)

DBI::dbIsValid(conn)

tables <- DBI::dbListTables(conn)
tables

olive_oil <- DBI::dbReadTable(conn, "olive_oil")

DBI::dbDisconnect(conn)

# EDA 
names(olive_oil)
head(olive_oil)
str(olive_oil)

# check for incomplete data 
skim(olive_oil)

# Histogram of each numerical variable
numeric_cols <- names(olive_oil)[sapply(olive_oil, is.numeric)]


olive_long <- pivot_longer(
  olive_oil,
  cols = all_of(numeric_cols),
  names_to = "variable",
  values_to = "value"
)

# Faceted histogram
ggplot(olive_long, aes(x = value)) +
  geom_histogram(binwidth = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal()




# Scale numeric data
olive_scaled <- scale(olive_oil[, numeric_cols])

# Step 1: PCA
pca <- prcomp(olive_scaled, center = TRUE, scale. = TRUE)

# Visualize explained variance
fviz_eig(pca)

# Project onto first 2 principal components
pca_df <- as.data.frame(pca$x[, 1:2])
colnames(pca_df) <- c("PC1", "PC2")

# Step 2: K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(pca_df, centers = 3, nstart = 25)

# Add cluster labels to PCA data
pca_df$cluster <- as.factor(kmeans_result$cluster)

# Visualize k-means clusters
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "K-Means Clustering on PCA-Reduced Olive Oil Data") +
  theme_minimal()

kNNdistplot(pca_df[, 1:2], k = 5)
abline(h = 1.0, col = "red", lty = 2)

# Step 3: DBSCAN Clustering (on scaled PCA)
dbscan_result <- dbscan(pca_df[, 1:2], eps = 0.3, minPts = 5)

# Add DBSCAN cluster labels
pca_df$dbscan <- as.factor(ifelse(dbscan_result$cluster == 0, "Noise", dbscan_result$cluster))

# Visualize DBSCAN clusters
ggplot(pca_df, aes(x = PC1, y = PC2, color = dbscan)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "DBSCAN Clustering on PCA-Reduced Olive Oil Data") +
  theme_minimal()
