# Load necessary libraries

#install.packages(ggplot2)
#install.packages(lubridate)
#install.packages(reshape2)
library(ggplot2)
library(lubridate)
library(reshape2)

# Load the dataset
data <- read.csv("Admissions_Pollution_Sorted.csv")

# Convert the Date column to Date format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Update these variables based on actual column names
admissions_column <- "Total.Admissions"  # Correct name for hospital admissions
date_column <- "Date"  # Correct name for the date column
air_quality_features <- colnames(data)[!(colnames(data) %in% c(date_column, admissions_column))]

# Generate summary statistics for all numeric features
summary_stats <- summary(data)

# Print the summary statistics
print(summary_stats)

# Define the target column and exclude it from the features
target_column <- "Total.Admissions"
date_column <- "Date"
numeric_features <- colnames(data)[sapply(data, is.numeric) & !(colnames(data) %in% c(date_column, target_column))]

# Calculate correlations
correlations <- sapply(numeric_features, function(feature) {
  cor(data[[feature]], data[[target_column]], use = "complete.obs")
})

# Print correlations
correlation_results <- data.frame(
  Feature = names(correlations),
  Correlation = correlations
)

print(correlation_results)

# Create the directory for saving plots if it doesn't exist
output_dir <- "Exploratory_Analysis"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Time series plots against target variable
for (feature in air_quality_features) {
  # Create the plot
  p <- ggplot(data) +
    geom_line(aes(x = .data[[date_column]], y = .data[[admissions_column]], color = "Total Admissions"), linewidth = 1, linetype = "solid") +
    geom_line(aes(x = .data[[date_column]], y = .data[[feature]], color = "Feature"), linewidth = 1, linetype = "dashed") +
    labs(title = paste("Trends in Total Admissions and", feature),
         x = "Date",
         y = "Counts / Index",
         color = "Legend") +
    scale_color_manual(
      values = c("Total Admissions" = "orange", "Feature" = "red")
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print the plot
  print(p)

}
# Correlation Matrix (Heat Map)
numeric_data <- data[sapply(data, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix for heatmap plotting
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
heatmap_plot <- ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  labs(title = "Correlation Matrix Heatmap", x = "Features", y = "Features") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the heatmap
print(heatmap_plot)


# Box Plots
for (feature in numeric_features) {
  # Create the box plot
  box_plot <- ggplot(data, aes(y = .data[[feature]])) +
    geom_boxplot(fill = "skyblue", color = "darkblue") +
    labs(title = paste("Box Plot of", feature),
         y = feature,
         x = "") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print the box plot
  print(box_plot)

}
  

# Scatter Plots
for (feature in numeric_features) {
  # Create the scatter plot
  scatter_plot <- ggplot(data, aes(x = .data[[feature]], y = .data[[target_column]])) +
    geom_point(color = "darkred", alpha = 0.6) +
    labs(title = paste("Scatter Plot of", feature, "vs", target_column),
         x = feature,
         y = target_column) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print the scatter plot
  print(scatter_plot)
  
}
  

