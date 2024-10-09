# Load required libraries
library(ggplot2)
library(dplyr)

# Sample data
data <- data.frame(
  Name = c("Andrew (A)", "Bernhard (B)", "Carolina (C)", "Dennis (D)", "Eve (E)", "Fred (F)"),
  Age = c(55, 43, 37, 82, 23, 46),
  Educational_Level = c(1, 2, 5, 3, 3.2, 5)
)

# Define initial centroids (Andrew and Carolina)
initial_centroids <- data.frame(
  Name = c("Andrew (A)", "Carolina (C)"),
  Age = c(55, 37),
  Educational_Level = c(1, 5)
)

# Create a dummy dataframe for legend
legend_df <- rbind(
  data %>% mutate(Type = "Data Points"),
  initial_centroids %>% mutate(Type = "Centroids")
)

# Plot dataset with initial centroids
plot1 <- ggplot(data, aes(x=Age, y=Educational_Level)) +
  geom_point(aes(color='Data Points')) +  # Specify color inside aes() function
  geom_point(data=initial_centroids, aes(shape='Centroids'), color='red', size=5, show.legend = "legend_only") +
  geom_text(data=initial_centroids, aes(label = "+"), color='red', size=8, vjust=0.5, hjust=0.5) +  # Add plus sign
  labs(x="Age", y="Educational Level", title="Dataset with Initial Centroids") + 
  scale_color_manual(values=c("black", "red"), guide=guide_legend(title=NULL)) +  # Set custom colors and remove legend title
  scale_shape_manual(values=c(NA, NA), guide=guide_legend(title=NULL)) +  # Remove shape legend
  theme_minimal()

# Show the first plot
print(plot1)

# K-means clustering with initial centroids
kmeans_data <- kmeans(data[,2:3], centers=initial_centroids[,2:3])

# Get cluster centers (recomputed centroids)
recomputed_centroids <- data.frame(kmeans_data$centers)

# Plot dataset and re-computed centroids
plot2 <- ggplot(data, aes(x=Age, y=Educational_Level)) +
  geom_point(aes(color=factor(kmeans_data$cluster))) +
  geom_point(data=recomputed_centroids, aes(shape='Centroids'), color='blue', size=5, show.legend = "legend_only") +
  geom_text(data=recomputed_centroids, aes(label = "+"), color='blue', size=8, vjust=0.5, hjust=0.5) +  # Add plus sign
  geom_point(data=data, aes(color=factor(kmeans_data$cluster)), show.legend = FALSE) +
  labs(x="Age", y="Educational Level", title="Dataset with Recomputed Centroids") +
  scale_color_manual(values=c("blue", "red"), guide=guide_legend(title=NULL)) +  # Set custom colors and remove legend title
  scale_shape_manual(values=c(NA, NA), guide=guide_legend(title=NULL)) +  # Remove shape legend
  theme_minimal() +
  guides(
    color = guide_legend(override.aes = list(shape = c(16, 16), size = c(3, 3), color = c("black", "black"))),
    shape = guide_legend(override.aes = list(color = c("red", "blue")))
  )

# Show the second plot
print(plot2)

