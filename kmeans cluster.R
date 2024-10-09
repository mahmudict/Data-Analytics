# Define the initial data
data <- data.frame(
  Name = c("Andrew", "Bernhard", "Carolina", "Dennis", "Eve", "Fred"),
  Age = c(55, 43, 37, 82, 23, 46),
  Educational_level = c(1, 2, 5, 3, 3.2, 5)
)

# Perform k-means clustering with 2 clusters and initial centroids
kmeans_initial <- kmeans(data[, c("Age", "Educational_level")], centers = data.frame(
  Age = c(55, 37),  # Initial centroids for Age
  Educational_level = c(1, 5)  # Initial centroids for Educational level
))

# Extract initial cluster assignments
initial_clusters <- kmeans_initial$cluster

# Plot initial centroids and data points
plot(data$Age, data$Educational_level, col = initial_clusters, pch = 19, 
     xlab = "Age", ylab = "Educational Level", main = "Initial Centroids")
points(kmeans_initial$centers[, 1], kmeans_initial$centers[, 2], col = 1:2, pch = 8, cex = 2)
legend("topright", legend = c("Cluster 1", "Cluster 2", "Centroid 1", "Centroid 2"), 
       col = c(1, 2, 1, 2), pch = c(19, 19, 8, 8), cex = 0.8, bg = "white", 
       pt.cex = c(1, 1, 2, 2), text.col = "black", horiz = TRUE, inset = 0.02)

# Perform k-means clustering with 2 clusters and recomputed centroids
kmeans_recomputed <- kmeans(data[, c("Age", "Educational_level")], centers = kmeans_initial$centers)

# Extract recomputed cluster assignments
recomputed_clusters <- kmeans_recomputed$cluster

# Plot recomputed centroids and data points
plot(data$Age, data$Educational_level, col = recomputed_clusters, pch = 19, 
     xlab = "Age", ylab = "Educational Level", main = "Recomputed Centroids")
points(kmeans_recomputed$centers[, 1], kmeans_recomputed$centers[, 2], col = 1:2, pch = 8, cex = 2)
legend("topright", legend = c("Cluster 1", "Cluster 2", "Centroid 1", "Centroid 2"), 
       col = c(1, 2, 1, 2), pch = c(19, 19, 8, 8), cex = 0.8, bg = "white", 
       pt.cex = c(1, 1, 2, 2), text.col = "black", horiz = TRUE, inset = 0.02)
