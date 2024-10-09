library(plot3D)
library(ggplot2)
setwd("D:/TU/Spring 2024/ISCS 539/Assignment/Assignment 3")



ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, size = Petal.Length, color = Species)) +
  geom_point() +
  scale_size_continuous(range = c(1, 3)) +  # Adjust the size range as needed
  labs(x = "Sepal Length", y = "Sepal Width", title = "Scatter Plot for Sepal Length, Sepal Width, and Petal Length") +
  theme_minimal()


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species, color = Species)) +
  geom_point(size = 3) +
  labs(x = "Sepal Length", y = "Sepal Width", title = "Scatter Plot for Sepal Length, Sepal Width, and Species") +
  theme_minimal() +
  scale_shape_manual(values = c(15, 15, 16)) +  # Customize shape values for each species
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#e31a1c"))  # Customize color values for each species

library(plot3D)

# Create a 3D scatter plot
s3d <- scatter3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, 
          colvar = as.integer(iris$Species),
          col = c("#1f78d1", "#33a01e", "#e3133c"),
          pch = 15,
          main = "3D Scatter Plot for Sepal Length, Sepal Width, and Petal Length",
          xlab = "Sepal Length",
          ylab = "Sepal Width",
          zlab = "Petal Length")
s3d$points3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, 
             col = as.integer(iris$Species), 
             pch = 15)

# Add a legend for species
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 16)





# Add a legend for species
legend("bottom", legend = levels(iris$Species), col = 1:3, lty = 1, lwd = 2)

#read a space-separated text file
data <- read.delim("iris")
#Create varialbes
Age <- data$age 
hist(Age, 
     main = "Histogram of Age",  # Title of the plot
     xlab = "Age",               # Label for the x-axis
     ylab = "Frequency",         # Label for the y-axis
     col = "skyblue",            # Fill color of the bars
     border = "black"            # Border color of the bars
)

library(GGally)
library(datasets)

# Load the Iris dataset
data(iris)

# Specify the color palette for species
color_palette <- c("#1b9e77", "#d95f02", "#7570b3")

# Create a parallel coordinates plot
p <- ggpairs(
  data = iris,
  columns = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  mapping = aes(color = Species),
  palette = color_palette
)

# Display the plot
print(p)
############################################





############################################
# Load the required library
library(GGally)

# Load the iris dataset
data(iris)

# Convert the "Species" variable to a factor
iris$Species <- as.factor(iris$Species)

# Create parallel coordinates plot using GGally
ggparcoord(
  data = iris,
  columns = 1:4,
  groupColumn = 'Species',
  showPoints = TRUE,
  title = "Parallel Coordinates Plot of Iris Data"
)

#####################################

# Load the required library
library(MASS)  # Required for the truehist function

# Function to calculate mode
get_mode <- function(x) {
  density_x <- density(x)
  mode <- density_x$x[which.max(density_x$y)]
  return(mode)
}

# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Minimum = sapply(iris[, 1:4], min),
  Maximum = sapply(iris[, 1:4], max),
  Average = sapply(iris[, 1:4], mean),
  Mode = sapply(iris[, 1:4], get_mode)
)

# Print the summary table
print(summary_table)
##################################
# Load the iris dataset
data(iris)

# Load the stats package for the mad function
library(stats)

# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Amplitude = sapply(iris[, 1:4], function(x) max(x) - min(x)),
  MeanAbsoluteDeviation = sapply(iris[, 1:4], mad),
  StandardDeviation = sapply(iris[, 1:4], sd)
)

# Print the summary table
print(summary_table)

#########################



# Load libraries
library(iris)
library(parallel)

# Select desired features and species
data <- iris[, c(1:4, 5)]  # Select Sepal Length, Sepal Width, Petal Length, Petal Width, and Species

# Define color mapping for species
colors <- c("red", "green", "blue")  # Colors for setosa, versicolor, virginica

# Create parallel coordinates plot
par(mfrow = c(1, 1), mar = c(4, 4, 1, 2))  # Set layout and margins

parallelcoords(data[, 1:4], col = colors[data$Species], 
               pch = ".", lwd = 0.5, las = 1)  # Plot lines, points, and labels

# Add labels to each axis
axis.by(1, paste(names(data)[1:4], collapse = "\n"))  # Add labels on left side

# Add legend for species
legend("topright", legend = names(levels(data$Species)), 
       fill = colors, title = "Species")  # Add legend



# Load libraries
library(iris)
library(parallel)

# Select desired features and species
data <- iris[, c(1:4, 5)]  # Select Sepal Length, Sepal Width, Petal Length, Petal Width, and Species

# Define color mapping for species
colors <- c("red", "green", "blue")  # Colors for setosa, versicolor, virginica

# Create parallel coordinates plot
par(mfrow = c(1, 1), mar = c(4, 4, 1, 2))  # Set layout and margins

parallelcoords(data[, 1:4], col = colors[data$Species], 
               pch = ".", lwd = 0.5, las = 1)  # Plot lines, points, and labels

# Add labels to each axis
axis.by(1, paste(names(data)[1:4], collapse = "\n"))  # Add labels on left side

# Add legend for species
legend("topright", legend = names(levels(data$Species)), 
       fill = colors, title = "Species")  # Add legend
                        
              
                                                                                                                                                                        # Convert the "Species" variable to a factor
                                                                                   
# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Minimum = sapply(iris[, 1:4], min),
  Maximum = sapply(iris[, 1:4], max),
  Average = sapply(iris[, 1:4], mean),
  Mode = sapply(iris[, 1:4], get_mode)
)

# Print the summary table
print(summary_table)


# Load the iris dataset
data(iris)

# Function to calculate mode
get_mode <- function(x) {
  bins <- seq(min(x), max(x), length.out = 30)  # Adjust the number of bins as needed
  hist_x <- hist(x, breaks = bins, plot = FALSE)
  mode_index <- which.max(hist_x$counts)
  mode <- hist_x$mids[mode_index]
  return(mode)
}

# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Minimum = sapply(iris[, 1:4], min),
  Maximum = sapply(iris[, 1:4], max),
  Average = sapply(iris[, 1:4], mean),
  Mode = sapply(iris[, 1:4], get_mode)
)

# Print the summary table
print(summary_table)

Copy code
# Load the required library
library(MASS)  # Required for the truehist function

# Function to calculate mode
get_mode <- function(x) {
  density_x <- density(x)
  mode <- density_x$x[which.max(density_x$y)]
  return(mode)
}

# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Minimum = sapply(iris[, 1:4], min),
  Maximum = sapply(iris[, 1:4], max),
  Average = sapply(iris[, 1:4], mean),
  Mode = sapply(iris[, 1:4], get_mode)
)

# Print the summary table
print(summary_table)

# Load the iris dataset
data(iris)

# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Amplitude = sapply(iris[, 1:4], function(x) diff(range(x))),
  MeanAbsoluteDeviation = sapply(iris[, 1:4], mad),
  StandardDeviation = sapply(iris[, 1:4], sd)
)

# Print the summary table
print(summary_table)

  



# Load the iris dataset
data(iris)

# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Amplitude = sapply(iris[, 1:4], function(x) max(x) - min(x)),
  MeanAbsoluteDeviation = sapply(iris[, 1:4], mad),
  StandardDeviation = sapply(iris[, 1:4], sd)
)

# Print the summary table
print(summary_table)

# Load the iris dataset
data(iris)

# Load the stats package for the mad function
library(stats)

# Calculate statistics
summary_table <- data.frame(
  Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  Amplitude = sapply(iris[, 1:4], function(x) max(x) - min(x)),
  MeanAbsoluteDeviation = sapply(iris[, 1:4], mad),
  StandardDeviation = sapply(iris[, 1:4], sd)
)

# Print the summary table
print(summary_table)

#######################

# Load the required library
library(GGally)

# Load the iris dataset
data(iris)

# Create scatter plot matrix with correlation coefficients
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.5),
    combo = wrap("facethist", binwidth = 0.2)
  ),
  diag = list(continuous = wrap("barDiag", binwidth = 0.2))
)

# Add correlation coefficients as text to upper plots
scatter_matrix$plots[[1, 2]] <- scatter_matrix$plots[[1, 2]] +
  theme(axis.text.x = element_text(size = 8)) +
  geom_text(data = data.frame(), aes(x = 0, y = 1, label = paste("r =", round(cor(iris$Sepal.Length, iris$Sepal.Width), digits = 2))), vjust = 1, hjust = 0)

scatter_matrix$plots[[1, 3]] <- scatter_matrix$plots[[1, 3]] +
  theme(axis.text.x = element_text(size = 8)) +
  geom_text(data = data.frame(), aes(x = 0, y = 1, label = paste("r =", round(cor(iris$Sepal.Length, iris$Petal.Length), digits = 2))), vjust = 1, hjust = 0)

scatter_matrix$plots[[1, 4]] <- scatter_matrix$plots[[1, 4]] +
  theme(axis.text.x = element_text(size = 8)) +
  geom_text(data = data.frame(), aes(x = 0, y = 1, label = paste("r =", round(cor(iris$Sepal.Length, iris$Petal.Width), digits = 2))), vjust = 1, hjust = 0)

scatter_matrix$plots[[2, 3]] <- scatter_matrix$plots[[2, 3]] +
  theme(axis.text.x = element_text(size = 8)) +
  geom_text(data = data.frame(), aes(x = 0, y = 1, label = paste("r =", round(cor(iris$Sepal.Width, iris$Petal.Length), digits = 2))), vjust = 1, hjust = 0)

scatter_matrix$plots[[2, 4]] <- scatter_matrix$plots[[2, 4]] +
  theme(axis.text.x = element_text(size = 8)) +
  geom_text(data = data.frame(), aes(x = 0, y = 1, label = paste("r =", round(cor(iris$Sepal.Width, iris$Petal.Width), digits = 2))), vjust = 1, hjust = 0)

scatter_matrix$plots[[3, 4]] <- scatter_matrix$plots[[3, 4]] +
  theme(axis.text.x = element_text(size = 8)) +
  geom_text(data = data.frame(), aes(x = 0, y = 1, label = paste("r =", round(cor(iris$Petal.Length, iris$Petal.Width), digits = 2))), vjust = 1, hjust = 0)

# Print the scatter plot matrix
print(scatter_matrix)
  


##########################

library(GGally)
library(stars)

# Subset the first 20 rows of the iris dataset
iris_subset <- iris[1:20, ]

# Create a parallel coordinates plot using ggparcoord
ggparcoord(data = iris_subset, columns = 1:4, groupColumn = "Species")

# Create a star plot using stars
stars(iris_subset[, 1:4])

# Load the iris dataset
data(iris)

# Specify the columns of interest
columns_of_interest <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Function to calculate mode
calculate_mode <- function(x) {
  tab <- table(x)
  mode_values <- as.numeric(names(tab)[tab == max(tab)])
  return(mode_values)
}

# Calculate summary statistics
summary_stats <- data.frame(
  Attribute = character(),
  Min = numeric(),
  Max = numeric(),
  Mean = numeric(),
  Mode = I(list()),
  stringsAsFactors = FALSE
)

for (col in columns_of_interest) {
  summary_stats <- rbind(
    summary_stats,
    data.frame(
      Attribute = col,
      Min = min(iris[[col]]),
      Max = max(iris[[col]]),
      Mean = mean(iris[[col]]),
      Mode = calculate_mode(iris[[col]])
    )
  )
}

# Print the results
print(summary_stats)

# Calculate amplitude, mean absolute deviation (MAD), and standard deviation for each column
summary_stats <- data.frame(
  Attribute = character(),
  Amplitude = numeric(),
  MAD = numeric(),
  SD = numeric(),
  stringsAsFactors = FALSE
)

for (col in columns_of_interest) {
  summary_stats <- rbind(
    summary_stats,
    data.frame(
      Attribute = col,
      Amplitude = max(iris[[col]]) - min(iris[[col]]),
      MAD = mean(abs(iris[[col]] - mean(iris[[col]]))),
      SD = sd(iris[[col]])
    )
  )
}

# Print the results
print(summary_stats)

# Install and load the aplpack package
install.packages("aplpack")
library(aplpack)

# Load the iris dataset
data(iris)

# Select the first 20 rows and the specified columns
iris_subset <- iris[1:20, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]

# Load the faces function from aplpack
faces <- aplpack::faces

# Create Chernoff Faces with names
faces(
  iris_subset,
  face.type = 1,
  labels = rownames(iris_subset),
  main = "Chernoff Faces for Iris Dataset (First 20)"
)

# Specify the columns of interest
columns_of_interest <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Create a boxplot for each column
boxplot(iris[, columns_of_interest], 
        col = c("blue", "green", "coral", "yellow"),
        main = "Boxplot for Iris Dataset",
        names = columns_of_interest,
        ylab = "Measurement",
        xlab = "Attributes")


# Specify the columns of interest
columns_of_interest <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Create a scatter plot matrix
pairs(iris[, columns_of_interest], 
      pch = 16, 
      col = iris$Species,
      main = "Scatter Plot Matrix with Pearson Correlation Coefficients")

# Add Pearson correlation coefficients to the plot
cor_matrix <- cor(iris[, columns_of_interest])
text(x = rep(1:length(columns_of_interest), each = length(columns_of_interest)),
     y = rep(length(columns_of_interest):1, length(columns_of_interest)),
     labels = sprintf("%.2f", cor_matrix),
     pos = 4, col = "red")

# Install and load necessary packages
install.packages("corrplot")
library(corrplot)

# Load the Iris dataset
data(iris)

# Extract the relevant columns
iris_data <- iris[, 1:4]

# Compute the correlation matrix
cor_matrix <- cor(iris_data)

# Create a correlogram using corrplot
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         addrect = 2, tl.col = "black", tl.srt = 45)

# Add a title
mtext("Correlogram for Iris Dataset", side = 1, line = 4, cex = 1.2, adj = 0.3)
# install.packages("gplots")

install.packages("pheatmap")

library(pheatmap)

# Subset the relevant columns from the iris dataset
iris_subset <- iris[, 1:4]

# Define custom color palettes for each column
color_palettes <- list(
  Sepal.Length = colorRampPalette(c("lightblue", "blue"))(100),
  Sepal.Width = colorRampPalette(c("lightgreen", "green"))(100),
  Petal.Length = colorRampPalette(c("lightcoral", "red"))(100),
  Petal.Width = colorRampPalette(c("lightyellow", "yellow"))(100)
)


# Create a matrix of colors for the 'color' argument
color_matrix <- do.call(cbind, color_palettes)

# Create a heatmap using pheatmap
pheatmap(iris_subset, 
         main = "Heatmap for Iris Dataset",
         cluster_rows = TRUE,  # Add row dendrogram
         scale = "row",        # Scale rows (optional)
         color = color_matrix,  # Use matrix of colors
         fontsize_row = 8, fontsize_col = 8  # Adjust text size
)


library(pheatmap)

# Subset the relevant columns from the iris dataset
iris_subset <- iris[, 1:4]

# Define custom color palettes for each column
color_palettes <- list(
  Sepal.Length = colorRampPalette(c("lightblue", "blue"))(100),
  Sepal.Width = colorRampPalette(c("lightgreen", "green"))(100),
  Petal.Length = colorRampPalette(c("lightcoral", "red"))(100),
  Petal.Width = colorRampPalette(c("lightyellow", "yellow"))(100)
)

# Create the heatmap with x-axis labels
pheatmap(
  iris_subset,
  main = "Iris Dataset Heatmap",
  show_colnames = FALSE,
  breaks = seq(min(iris_subset), max(iris_subset), length = 11),
  color = color_palettes,
  # Add x-axis labels with rownames of iris_subset
  xlab = rownames(iris_subset)
)



Plas <- data$plas
Pres <- data$pres

boxplot(Plas, Pres,
        main = "Boxplot of plas and pres",
        names = c("plas", "pres"),
        col = c("yellow", "lightgreen"),
        border = "black",
        notch = TRUE,
        ylab = "Values"
)

boxplot(Plas, Pres,
        main = "Boxplot of Plas and Pres",
        names = c("Plas", "Pres"),
        col = c("blue", "red"),
        border = "black",
        notch = TRUE,
        ylab = "Values",
        xlab = "Variables"
)


plot(Plas, Pres, pch = 19, col = c("blue", "red"), main = "Scatter Plot", xlab = "Plas", ylab = "Pres")
legend("bottomleft", legend = c("Plas", "Pres"), col = c("blue", "red"), pch = 19)

data <- read.delim("weather")
Outlook <- data$outlook

# Create a table to get the frequency of each category
outlook_counts <- table(Outlook)

# Create a pie chart
pie(outlook_counts, 
    main = "Pie Chart of Outlook",
    labels = paste(names(outlook_counts), ": ", outlook_counts),
    col = rainbow(length(outlook_counts))
)

data <- read.delim("weather")

# Create a contingency table
table_data <- table(data$outlook, data$windy)

# Convert the table to a data frame
df <- as.data.frame.matrix(table_data)

# Rename the columns for clarity
colnames(df) <- c("FALSE", "TRUE")

# Plot the stacked barplot using ggplot2
library(ggplot2)

ggplot(df, aes(x = rownames(df), y = TRUE, fill = "TRUE")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(x = rownames(df), y = FALSE, fill = "FALSE"), stat = "identity") +
  labs(title = "Stacked Barplot of Outlook and Windy",
       x = "Outlook", y = "Frequency",
       fill = "Windy") +
  theme_minimal()







# Create a contingency table
contingency_table <- table(data$outlook, data$windy)

# Display the contingency table
print(contingency_table)


# Convert the contingency table to a data frame for easier manipulation
contingency_df <- as.data.frame.matrix(contingency_table)

colors <- c("lightblue", "lightgreen", "lightyellow")

# Create a mosaic plot with custom colors
mosaicplot(as.matrix(contingency_df), 
           main = "Mosaic Plot for Outlook and Windy",
           color = colors,
           off = 1,  # Specify color for "Off" region (background)
           las = 1   # Make axis labels horizontal
)



# install.packages("ggplot2")
library(ggplot2)


# Create a ggplot object for a mosaic plot with custom colors
ggplot(data, aes(x = windy, fill = outlook)) +
  geom_bar(position = "fill") +
  labs(title = "Mosaic Plot for Outlook and Windy",
       fill = "Outlook",
       x = "Windy",
       y = "Proportion") +
  scale_fill_manual(values = c("blue", "green", "red")) +
  theme_minimal()

ggplot(data, aes(x = windy, fill = outlook)) +
  geom_bar() +
  labs(title = "Bar Plot for Outlook and Windy",
       fill = "Outlook",
       x = "Windy",
       y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightyellow")) +
  theme_minimal()

# Create a ggplot object for a stacked bar plot with custom colors
ggplot(data, aes(x = windy, fill = outlook)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot for Outlook and Windy",
       fill = "Outlook",
       x = "Windy",
       y = "Count") +
  scale_fill_manual(values = c("blue", "green", "red")) +
  theme_minimal()

#Create varialbes
Weight <- data$Weight
Gender <- data$Gender
Company <- data$Company
Height <- data$Height

name= data$Friend
# histogram for a numeric attribute
hist(Weight)

# bar plot for a numeric attribute
barplot(Weight, main="Weight",
        xlab="Weight")

# To create a frequency table
Gender_table <- table(Gender)
# pie plot for a categorical attribute
pie(Gender_table)


# bar plot for a categorical attribute
barplot(Gender_table, xlab = "Gender", ylab = "Frequency", 
        main = "Bar plot of Gender", names.arg=c("M","F"))

# stacked bar plot for two categorical attributes
Gender_Company <- table(Gender, Company)
barplot(Gender_Company, xlab = "Company", ylab = "Frequency", 
         main = "Stacked barchart", names.arg=c("Good","Bad"),
         col=c("darkblue","red"), legend.text = rownames(Gender_Company))

# box plot for a numeric attribute
boxplot(Weight)

boxplot(Weight, horizontal = TRUE, ylab = "Weight")

# scatter plot for the two numeric attributes
plot(Weight, Height)

# install.packages("plot3D")
weight_height <- table(Weight, Height)
# 3D histogram
hist3D(z=weight_height, border="black", main = "3D Histogram", 
       xlab = "Weight", ylab = "Height", zlab = "Frequency")#######

##################################


# Load the required library
library(ggplot2)

# Load the iris dataset
data(iris)

# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Function to add correlation coefficients as text annotations
add_correlation_text <- function(x, y, cor_value) {
  annotate(
    "text",
    x = mean(range(x)),
    y = mean(range(y)),
    label = sprintf("r = %.2f", cor_value),
    size = 3,
    hjust = 1,
    vjust = 0,
    color = "red"
  )
}

# Create scatter plot matrix
scatter_matrix <- ggplot(iris, aes(color = Species)) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width), alpha = 0.5) +
  add_correlation_text(iris$Sepal.Length, iris$Sepal.Width, cor_coef[1, 2]) +
  
  geom_point(aes(x = Sepal.Length, y = Petal.Length), alpha = 0.5) +
  add_correlation_text(iris$Sepal.Length, iris$Petal.Length, cor_coef[1, 3]) +
  
  geom_point(aes(x = Sepal.Length, y = Petal.Width), alpha = 0.5) +
  add_correlation_text(iris$Sepal.Length, iris$Petal.Width, cor_coef[1, 4]) +
  
  geom_point(aes(x = Sepal.Width, y = Petal.Length), alpha = 0.5) +
  add_correlation_text(iris$Sepal.Width, iris$Petal.Length, cor_coef[2, 3]) +
  
  geom_point(aes(x = Sepal.Width, y = Petal.Width), alpha = 0.5) +
  add_correlation_text(iris$Sepal.Width, iris$Petal.Width, cor_coef[2, 4]) +
  
  geom_point(aes(x = Petal.Length, y = Petal.Width), alpha = 0.5) +
  add_correlation_text(iris$Petal.Length, iris$Petal.Width, cor_coef[3, 4]) +
  
  labs(title = "Scatter Plot Matrix with Correlation Coefficients") +
  theme_minimal()

# Print the scatter plot matrix
print(scatter_matrix)
##################################################

# Load the required libraries
library(GGally)
library(ggplot2)

# Load the iris dataset
data(iris)

# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Function to add correlation coefficients as text annotations
add_correlation_text <- function(x, y, cor_value) {
  annotate(
    "text",
    x = mean(range(x)),
    y = mean(range(y)),
    label = sprintf("r = %.2f", cor_value),
    size = 3,
    hjust = 1,
    vjust = 0,
    color = "red"
  )
}

# Create scatter plot matrix
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.5),
    combo = wrap("facethist", binwidth = 0.2)
  )
)

# Add correlation coefficients as text annotations
for (i in 1:3) {
  for (j in (i + 1):4) {
    scatter_matrix <- scatter_matrix + 
      add_correlation_text(iris[, i], iris[, j], cor_coef[i, j])
  }
}

# Print the scatter plot matrix
print(scatter_matrix)
######################################################

# Load the required libraries
library(GGally)
library(ggplot2)

# Load the iris dataset
data(iris)

# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Function to add correlation coefficients as text annotations
add_correlation_text <- function(x, y, cor_value) {
  annotate(
    "text",
    x = mean(range(x)),
    y = mean(range(y)),
    label = sprintf("r = %.2f", cor_value),
    size = 4,
    hjust = 1,
    vjust = 0,
    color = "darkblue",
    family = "sans-serif"
  )
}

# Create scatter plot matrix
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.5),
    combo = wrap("facethist", binwidth = 0.2)
  ),
  mapping = aes(color = Species)  # Add color mapping
)

# Add correlation coefficients as text annotations
for (i in 1:3) {
  for (j in (i + 1):4) {
    scatter_matrix <- scatter_matrix + 
      add_correlation_text(iris[, i], iris[, j], cor_coef[i, j])
  }
}

# Customize theme for better visibility
scatter_matrix <- scatter_matrix +
  theme_minimal() +
  theme(text = element_text(family = "sans-serif", size = 10),
        axis.text = element_text(size = 8))

# Print the scatter plot matrix
print(scatter_matrix)
#################################################


# Load the required libraries
library(GGally)
library(ggplot2)

# Load the iris dataset
data(iris)

# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Function to add correlation coefficients as text annotations
add_correlation_text <- function(x, y, cor_value) {
  annotate(
    "text",
    x = mean(range(x)),
    y = mean(range(y)),
    label = sprintf("r = %.2f", cor_value),
    size = 3.5,
    hjust = 1,
    vjust = 0,
    color = "darkblue",
    family = "sans-serif"
  )
}

# Create scatter plot matrix
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.3),
    combo = wrap("facethist", binwidth = 0.15)
  ),
  mapping = aes(color = Species)  # Add color mapping
)

# Add correlation coefficients as text annotations
for (i in 1:3) {
  for (j in (i + 1):4) {
    scatter_matrix <- scatter_matrix + 
      add_correlation_text(iris[, i], iris[, j], cor_coef[i, j])
  }
}

# Customize theme for better visibility
scatter_matrix <- scatter_matrix +
  theme_minimal() +
  theme(text = element_text(family = "sans-serif", size = 10),
        axis.text = element_text(size = 8))

# Print the scatter plot matrix
print(scatter_matrix)

#############################################


# Load the required libraries
library(GGally)
library(ggplot2)

# Load the iris dataset
data(iris)
# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Function to add correlation coefficients as text annotations
add_correlation_text <- function(x, y, cor_value) {
  annotate(
    "text",
    x = mean(range(x)),
    y = mean(range(y)),
    label = sprintf("r = %.2f", cor_value),
    size = 3.5,
    hjust = 1,
    vjust = 0,
    color = "darkblue",
    family = "sans-serif"
  )
}

# Create scatter plot matrix with only scatter plots
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.3)
  ),
  mapping = aes(color = Species)  # Add color mapping
)

# Add correlation coefficients as text annotations
for (i in 1:3) {
  for (j in (i + 1):4) {
    scatter_matrix <- scatter_matrix + 
      add_correlation_text(iris[, i], iris[, j], cor_coef[i, j])
  }
}

# Customize theme for better visibility
scatter_matrix <- scatter_matrix +
  theme_minimal() +
  theme(text = element_text(family = "sans-serif", size = 10),
        axis.text = element_text(size = 8))

# Print the scatter plot matrix
print(scatter_matrix)
#########################################################

# Load the required libraries
library(GGally)
library(ggplot2)

# Load the iris dataset
data(iris)

# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Create scatter plot matrix with correlation coefficients
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.6),
    combo = wrap("facethist", binwidth = 0.2)
  ),
  mapping = aes(color = Species)  # Add color mapping
)

# Add correlation coefficients as text annotations
for (i in 1:3) {
  for (j in (i + 1):4) {
    scatter_matrix <- scatter_matrix + 
      annotate("text", x = mean(range(iris[, i])), y = mean(range(iris[, j])),
               label = sprintf("r = %.2f", cor_coef[i, j]),
               size = 3.5, color = "darkblue", hjust = 0)
  }
}

# Print the scatter plot matrix
print(scatter_matrix)
########################################

# Load the required libraries
library(GGally)
library(ggplot2)

# Load the iris dataset
data(iris)

# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Create scatter plot matrix with correlation coefficients
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.6),
    combo = wrap("facethist", binwidth = 0.2),
    cor = function(data, mapping, method, ...) {
      xvar <- as.character(mapping$x)
      yvar <- as.character(mapping$y)
      corr <- cor_coef[which(colnames(cor_coef) == xvar), which(colnames(cor_coef) == yvar)]
      annotate(
        "text", x = 0.5, y = 0.9, label = sprintf("r = %.2f", corr),
        size = 5, color = "darkblue", hjust = 0
      )
    }
  ),
  mapping = aes(color = Species),  # Add color mapping
  lower = list(combo = "dot")
)

# Add correlation coefficients as text annotations
for (i in 1:3) {
  scatter_matrix <- scatter_matrix + 
    geom_text(
      aes(x = mean(range(iris[, i])), y = mean(range(iris[, i + 1])), label = sprintf("r = %.2f", cor_coef[i, i + 1])),
      size = 2, color = "darkblue", hjust = 0
    )
}

# Print the scatter plot matrix
print(scatter_matrix)

#####################################

# Load necessary libraries
library(GGally)
library(ggplot2)

# Calculate correlation coefficients
cor_coef <- cor(iris[, 1:4])

# Create scatter plot matrix with correlation coefficients
scatter_matrix <- ggpairs(
  data = iris,
  columns = 1:4,
  title = "Scatter Plot Matrix with Correlation Coefficients",
  upper = list(
    continuous = wrap("points", alpha = 0.5),
    combo = wrap("facethist", binwidth = 0.2)
  ),
  mapping = aes(color = Species)
)

# Add correlation coefficients as text annotations
for (i in 1:4) {
  for (j in (i + 1):4) {
    text_x <- mean(range(iris[, i]))
    text_y <- mean(range(iris[, j]))
    text_label <- sprintf("r = %.2f", cor_coef[i, j])
    
    scatter_matrix <- scatter_matrix + 
      annotate("text", x = text_x, y = text_y, label = text_label, size = 3.5, color = "darkblue", hjust = 0)
  }
}

# Print the scatter plot matrix
print(scatter_matrix)

##########################################
library(gplots)

# Extract numeric columns
iris_numeric <- iris[, 1:4]

# Convert to matrix
dataM <- as.matrix(iris_numeric)

# Create row and column labels
row_labels <- rownames(iris_numeric)
col_labels <- colnames(iris_numeric)

# Create the heatmap with labels and adjusted margins
heatmap(dataM, cexCol = 1, cexRow = 1, labRow = row_labels, labCol = col_labels, main = "Heatmap of Iris Dataset", margins = c(10, 5))
#######################
# Load the required libraries





