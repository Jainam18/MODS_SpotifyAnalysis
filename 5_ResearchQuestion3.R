# Spotify analysis - Aanchal Malhotra
# Research question: Can we cluster songs into meaningful groups for music recommendation?

# Define required packages
required_packages <- c("dplyr", "ggplot2", "factoextra", "cluster", "caret", "tibble")

# Function to install and load packages
install_and_load <- function(packages) {
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    }
}

# Install and load the required packages
install_and_load(required_packages)

# Load the dataset
data <- read.csv("Cleaned_SpotifyTrack.csv")

# Convert to tibble for better handling
data <- as_tibble(data)

# Set seed for reproducibility
set.seed(123)

# Randomly sample 30,000 rows
data_subset <- data[sample(seq_len(nrow(data)), 10000), ]

# Select relevant features
features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")

# Subset the data
data_subset <- data_subset[features]

# Standardize (scale) the data
data_scaled <- scale(data_subset)

# Elbow method to determine optimal k
fviz_nbclust(data_scaled, kmeans, method = "wss", nstart = 25, iter.max = 100)

# Initialize silhouette score vector
sil_scores <- numeric(9)  # k = 2 to 10

# Compute silhouette scores for different k values
for (k in 2:10) {
  kmeans_result <- kmeans(data_scaled, centers = k, nstart = 25, iter.max = 100) 
  sil <- silhouette(kmeans_result$cluster, dist(data_scaled))
  sil_scores[k - 1] <- mean(sil[, 3])
}

# Plot silhouette scores
plot(2:10, sil_scores, type = "b", pch = 19, xlab = "Number of Clusters (k)", ylab = "Average Silhouette Score", main = "Silhouette Method for Optimal k")

# Perform k-means clustering with optimal k (assume k = 2)
optimal_k <- 2
kmeans_result <- kmeans(data_scaled, centers = optimal_k, nstart = 50, iter.max = 100, algorithm = "Lloyd")

# Print summary of clustering
print(kmeans_result)

# (between_SS / total_SS =  21.4 %)

# Print cluster centers
print(kmeans_result$centers)

# Print within-cluster sum of squares
print(kmeans_result$withinss)


# Print total within-cluster sum of squares
print(kmeans_result$tot.withinss)


# Print between-cluster sum of squares
print(kmeans_result$betweenss)


# Print cluster sizes
print(kmeans_result$size)


# Compute silhouette scores for the clustering result
silhouette_score <- silhouette(kmeans_result$cluster, dist(data_scaled))

# Plot the silhouette score
plot(silhouette_score, main = "Silhouette Plot for K-Means Clustering")

# Perform PCA
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Create a data frame with the first two principal components and cluster assignments
pca_data <- data.frame(pca_result$x[, 1:2])  # First two principal components
pca_data$cluster <- as.factor(kmeans_result$cluster)

# Plot PCA with clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
   geom_point() +
   labs(title = "PCA of Songs Clusters", x = "PC1", y = "PC2") +
   theme_minimal()


# Plot cluster centers
centers_df <- as.data.frame(kmeans_result$centers)
centers_df$cluster <- factor(1:optimal_k)
centers_long <- tidyr::pivot_longer(centers_df, -cluster, names_to = "feature", values_to = "value")

ggplot(centers_long, aes(x = feature, y = value, fill = cluster)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(title = "Feature Averages by Cluster", y = "Standardized Value", x = "Feature") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

print("Done")