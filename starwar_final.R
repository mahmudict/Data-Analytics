# Load necessary libraries
library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(tidyr) 

# Load the starwars dataset
data("starwars")

# Select only numeric columns
numeric_cols <- select_if(starwars, is.numeric)

# Calculate summary statistics for numeric columns
summary_stats <- sapply(numeric_cols, function(x) {
  c(min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE))
})

# Print the summary statistics
print(summary_stats)

# Create boxplot for all numeric columns
boxplot(numeric_cols, main = "Boxplot of Star Wars Dataset")

# Exclude rows where every column has NA values
numeric_cols_filtered <- numeric_cols %>%
  filter_all(any_vars(!is.na(.)))

# Replace missing values with column means
numeric_cols_imputed <- lapply(numeric_cols_filtered, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# Convert the list back to a data frame
numeric_cols_imputed <- as.data.frame(numeric_cols_imputed)

# Function to compute inertia
compute_inertia <- function(k, data) {
  kmeans_model <- kmeans(data, centers = k, nstart = 25)
  return(kmeans_model$tot.withinss)
}

# Compute inertia for different values of K
inertia_values <- sapply(2:5, function(k) compute_inertia(k, numeric_cols_imputed))

# Plot inertia values
plot(2:5, inertia_values, type = "b", pch = 19,
     xlab = "Number of Clusters (K)", ylab = "Inertia",
     main = "Inertia vs. Number of Clusters")

# Find the optimal K based on the elbow method
elbow_point <- function(x, y) {
  dx <- diff(x)
  dy <- -diff(y)
  slope <- dy / dx
  elbow <- which.max(slope) + 1
  return(elbow)
}

optimal_k <- elbow_point(2:5, inertia_values)
points(optimal_k, inertia_values[optimal_k - 1], col = "red", pch = 19)
text(optimal_k + 0.2, inertia_values[optimal_k - 1],
     labels = paste0("Optimal K = ", optimal_k), pos = 4, col = "red")

