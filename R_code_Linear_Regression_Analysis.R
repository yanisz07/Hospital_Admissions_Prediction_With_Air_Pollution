# Load necessary libraries

#install.packages(dplyr)
#install.packages(purrr)
#install.packages(ggplot2)
#install.packages(MASS)
library(dplyr)
library(purrr)
library(ggplot2)
library(MASS)


# Load the dataset
data <- read.csv("Admissions_Pollution_Sorted.csv")

# Convert the Date column to Date format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Define parameters
target_column <- "Total.Admissions"
date_column <- "Date"                
air_quality_features <- colnames(data)[!(colnames(data) %in% c(date_column, target_column))]  # Exclude Date and Target
max_lag <- 30  # Maximum number of lags to generate

# Generate lagged features for the last 30 days (including 0 lag)
lagged_data <- bind_cols(
  data[target_column],  # Add the target column
  map_dfc(air_quality_features, function(feature) {
    map_dfc(0:max_lag, function(lag) {
      tibble(!!paste0(feature, "_lag", lag) := lag(data[[feature]], lag))
    })
  }),
  data[date_column]  # Add the Date column
)

# Remove rows with Null values from lagged_data
lagged_data <- na.omit(lagged_data)

# Pre-selection of best lag for each feature
pre_selection <- lapply(air_quality_features, function(feature) {
  # Get all lagged versions of the feature
  lagged_cols <- grep(paste0("^", feature, "_lag"), colnames(lagged_data), value = TRUE)
  
  # Compute correlation of each lagged feature with the target
  correlations <- sapply(lagged_cols, function(col) {
    cor(lagged_data[[col]], lagged_data[[target_column]], use = "complete.obs")
  })
  
  # Select the lag with the highest absolute correlation with target
  best_lag <- names(which.max(abs(correlations)))
  list(feature = feature, best_lag = best_lag, correlation = correlations[best_lag])
})

# Convert the pre-selection results to a data frame
pre_selection_df <- do.call(rbind, lapply(pre_selection, as.data.frame))

# Create the final dataframe with best lagged features
final_data <- lagged_data %>%
  dplyr::select(Date = !!sym(date_column), Total.Admissions = !!sym(target_column)) %>%  # Include Date and Target
  bind_cols(
    lapply(pre_selection_df$best_lag, function(lagged_col) lagged_data[[lagged_col]]) %>%
      setNames(
        sapply(pre_selection_df$best_lag, function(lagged_name) {
          feature_parts <- strsplit(lagged_name, "_lag")[[1]]
          paste(feature_parts[1], "_lag", feature_parts[2], sep = "")
        })
      )
  )

# View the structure of the final dataframe
str(final_data)


# Plot 1: Correlation of each lagged variable (0-30 days) with the target
correlation_data <- do.call(rbind, lapply(air_quality_features, function(feature) {
  lagged_cols <- grep(paste0("^", feature, "_lag"), colnames(lagged_data), value = TRUE)
  correlations <- sapply(lagged_cols, function(col) {
    cor(lagged_data[[col]], lagged_data[[target_column]], use = "complete.obs")
  })
  data.frame(Feature = feature, Lag = 0:max_lag, Correlation = correlations)
}))

# Line plot for correlations
plot1 <- ggplot(correlation_data, aes(x = Lag, y = Correlation, color = Feature, group = Feature)) +
  geom_line() +
  labs(title = "Correlation of Lagged Variables with Target", x = "Lag (days)", y = "Correlation") +
  theme_minimal()

# Plot 2: Bar plot of pre-selected lagged variables
pre_selection_df$Lag <- as.numeric(gsub(".*_lag", "", pre_selection_df$best_lag))  # Extract lag number
pre_selection_df$FeatureLag <- paste(pre_selection_df$feature, "(Lag", pre_selection_df$Lag, ")")  # Combine feature and lag

plot2 <- ggplot(pre_selection_df, aes(x = reorder(FeatureLag, -correlation), y = correlation)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +  # Neutral color for bars
  labs(title = "Pre-Selected Lagged Variables and Correlation with Target",
       x = "Feature (with optimal lag)",
       y = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank())  

# Print plots
print(plot1)
print(plot2)


# Stepwise Regression (Feature Selection)

# Define the target variable and predictors
target_column <- "Total.Admissions"
predictors <- colnames(final_data)[!(colnames(final_data) %in% c("Date", target_column))]

# Create the formula for the full model
full_formula <- as.formula(paste(target_column, "~", paste(predictors, collapse = " + ")))

# Fit the initial (full) regression model
initial_model <- lm(full_formula, data = final_data)

# Perform stepwise regression to select features
stepwise_model <- stepAIC(initial_model, direction = "both", trace = FALSE, k=4)

# Summarize the stepwise-selected model
cat("Stepwise Model Summary:\n")
summary(stepwise_model)


# Extract residuals and fitted values
residuals <- residuals(stepwise_model)
fitted_values <- fitted(stepwise_model)

#Check Independence of Errors (Autocorrelation)

# Plot residuals over the order of observations
plot(residuals, type = "l", main = "Residuals Over Time", xlab = "Observation Index", ylab = "Residuals")
abline(h = 0, col = "red")

# Durbin-Watson test for autocorrelation
library(lmtest)
dw_test <- dwtest(stepwise_model)
cat("Durbin-Watson Test:\n")
print(dw_test)

#Check Homoscedasticity (Constant Variance)

# Residuals vs Fitted Values Plot
ggplot(data.frame(Fitted = fitted_values, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

#Check Normality of Errors

# Histogram of Residuals
ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Q-Q Plot of Residuals
qqnorm(residuals)
qqline(residuals, col = "red")

