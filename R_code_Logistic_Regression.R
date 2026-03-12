# Load necessary libraries

#install.packages(dplyr)
#install.packages(pROC)
#install.packages(caret)
#install.packages(purrr)
#install.packages(ggplot2)
#install.packages(knitr)
#install.packages(kableExtra)
library(dplyr)
library(pROC)
library(caret)
library(purrr)
library(ggplot2)
library(knitr)    # For generating tables
library(kableExtra)  # For enhancing table output

# Transformation of the target into binary class

# Load the dataset
data <- read.csv("Admissions_Pollution_Sorted.csv")

# Convert the Date column to Date format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Define the target and date columns
target_column <- "Total.Admissions"
date_column <- "Date"

# Define the threshold for binary classification
threshold <- quantile(data[[target_column]], probs = 2/3)

# Create the binary target variable
data$High_Admissions <- ifelse(data[[target_column]] > threshold, 1, 0)

# Check the distribution of the binary target variable
table(data$High_Admissions)

# Pre-selection of the best lag for each feature using correlation

# Define air quality features
air_quality_features <- colnames(data)[!(colnames(data) %in% c(date_column, target_column, "High_Admissions"))]
max_lag <- 30  # Maximum number of lags to generate

# Generate lagged features for all air quality features
lagged_data <- bind_cols(
  data["High_Admissions"],  # Add the binary target column
  map_dfc(air_quality_features, function(feature) {
    map_dfc(0:max_lag, function(lag) {
      tibble(!!paste0(feature, "_lag", lag) := lag(data[[feature]], lag))
    })
  }),
  data[date_column]  # Add the Date column
)

# Remove rows with NA values due to the lagged features
lagged_data <- na.omit(lagged_data)

# Pre-select the best lag for each feature based on correlation
pre_selection <- lapply(air_quality_features, function(feature) {
  # Get all lagged versions of the feature
  lagged_cols <- grep(paste0("^", feature, "_lag"), colnames(lagged_data), value = TRUE)
  
  # Compute correlation of each lagged feature with the binary target
  correlations <- sapply(lagged_cols, function(col) {
    cor(lagged_data[[col]], lagged_data$High_Admissions, use = "complete.obs")
  })
  
  # Select the lag with the highest absolute correlation with binary target
  best_lag <- names(which.max(abs(correlations)))
  list(feature = feature, best_lag = best_lag, correlation = correlations[best_lag])
})

# Convert the pre-selection results to a data frame
pre_selection_df <- do.call(rbind, lapply(pre_selection, as.data.frame))

# 3) Generation of the dataset with only the pre-selected features

# Create the final dataset with pre-selected features
final_data <- lagged_data %>%
  dplyr::select(Date = !!sym(date_column), High_Admissions) %>%  # Include Date and Binary Target
  bind_cols(
    lapply(pre_selection_df$best_lag, function(lagged_col) lagged_data[[lagged_col]]) %>%
      setNames(
        sapply(pre_selection_df$best_lag, function(lagged_name) {
          # Extract the original feature name and lag
          feature_parts <- strsplit(lagged_name, "_lag")[[1]]
          paste(feature_parts[1], "_lag", feature_parts[2], sep = "")
        })
      )
  )

# View the structure of the final dataset
str(final_data)

# Prepare pre-selection data for plotting
pre_selection_df$Lag <- as.numeric(gsub(".*_lag", "", pre_selection_df$best_lag))  # Extract lag number
pre_selection_df$Feature_With_Lag <- paste0(pre_selection_df$feature, " (Lag ", pre_selection_df$Lag, ")")  # Add lag to feature name

# Plot:
plot_logreg <- ggplot(pre_selection_df, aes(x = reorder(Feature_With_Lag, -correlation), y = correlation)) +
  geom_bar(stat = "identity", fill = "gray") + # Neutral color for a professional look
  labs(title = "Pre-Selected Lagged Variables and Correlation with Target (Logistic Regression)",
       x = "Feature (with optimal lag)",
       y = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Tilt labels for better visibility
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# Print the plot
print(plot_logreg)



# Greedy Algorithm for Feature Selection with Stopping Criterion

# Prepare the predictors and response
predictors <- colnames(final_data)[!(colnames(final_data) %in% c("Date", "High_Admissions"))]
response <- final_data$High_Admissions

# Initialize the greedy algorithm
selected_features <- c()  # Start with an empty model
remaining_features <- predictors
best_likelihood <- -Inf  # Track the best likelihood
previous_likelihood <- -Inf  # Track the likelihood before the current step
improvement_threshold <- 1.0  # Minimum improvement in likelihood to add a feature

# Iterative greedy feature selection
while (length(remaining_features) > 0) {
  likelihoods <- c()  # Store likelihoods for each feature
  
  # Test adding each feature to the model
  for (feature in remaining_features) {
    # Create formula with the current selected features + this feature
    formula <- as.formula(paste("High_Admissions ~", paste(c(selected_features, feature), collapse = " + ")))
    model <- glm(formula, data = final_data, family = binomial)
    likelihoods[feature] <- logLik(model)  # Store the likelihood
  }
  
  # Find the feature that gives the best improvement
  best_feature <- names(which.max(likelihoods))
  best_feature_likelihood <- max(likelihoods)
  
  # Check if adding the feature improves the model significantly
  improvement <- best_feature_likelihood - previous_likelihood  # Calculate improvement
  
  # Check if the improvement is significant
  if (improvement > improvement_threshold) {
    # Update the model
    selected_features <- c(selected_features, best_feature)
    cat("Added feature:", best_feature, "with improvement:", improvement, "\n")
    remaining_features <- setdiff(remaining_features, best_feature)
    previous_likelihood <- best_feature_likelihood
  } else {
    # Stop if no significant improvement
    break
  }
}


# Add F1 Score Calculation
calculate_f1 <- function(conf_matrix) {
  precision <- conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[1, 2])
  recall <- conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[2, 1])
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  return(f1_score)
}

# Split the data into training and testing sets (80-20 split)
set.seed(42)  # Set seed for reproducibility
train_indices <- createDataPartition(final_data$High_Admissions, p = 0.8, list = FALSE)
train_data <- final_data[train_indices, ]
test_data <- final_data[-train_indices, ]

# Final model with selected features on training data
final_formula <- as.formula(paste("High_Admissions ~", paste(selected_features, collapse = " + ")))
final_model <- glm(final_formula, data = train_data, family = binomial)

# Summarize the final logistic regression model
cat("Final Logistic Model Summary (Training Data):\n")
summary(final_model)

# Predict probabilities on the test set
predicted_probabilities <- predict(final_model, test_data, type = "response")

# Convert probabilities to binary predictions
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)


#Plot Confusion Matrix

# Create the confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$High_Admissions)

conf_matrix_df <- as.data.frame(as.table(confusion_matrix))
colnames(conf_matrix_df) <- c("Prediction", "Actual", "Freq")

conf_matrix_df$Prediction <- factor(conf_matrix_df$Prediction, levels = rev(levels(factor(conf_matrix_df$Prediction))))

ggplot(conf_matrix_df, aes(x = Actual, y = Prediction)) +
  geom_tile(fill = "white", color = "black") + # White tiles with black borders
  geom_text(aes(label = Freq), color = "black", size = 6) + # Black text for numbers
  labs(title = "Confusion Matrix", x = "Actual", y = "Prediction") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.title = element_text(size = 12, face = "bold"), # Bold axis titles
    axis.text = element_text(size = 10) # Standardized axis text
  )
# Accuracy
accuracy <- mean(predicted_classes == test_data$High_Admissions)
cat("Accuracy (Test Data):", accuracy, "\n")

# F1 Score
f1_score <- calculate_f1(confusion_matrix)
cat("F1 Score (Test Data):", f1_score, "\n")

# ROC-AUC
roc_curve <- roc(test_data$High_Admissions, predicted_probabilities)
auc_value <- auc(roc_curve)
cat("ROC-AUC (Test Data):", auc_value, "\n")

# Plot ROC Curve
plot(roc_curve, main = "ROC Curve (Test Data)")



# Cross Validation

k <- 5  # Number of folds
folds <- createFolds(final_data$High_Admissions, k = k, list = TRUE)

cv_accuracy <- numeric(k)
cv_auc <- numeric(k)
cv_f1 <- numeric(k)

# Perform cross-validation
for (i in seq_along(folds)) {
  test_indices <- folds[[i]]
  train_data <- final_data[-test_indices, ]
  test_data <- final_data[test_indices, ]
  
  train_formula <- as.formula(paste("High_Admissions ~", paste(selected_features, collapse = " + ")))
  cv_model <- glm(train_formula, data = train_data, family = binomial)
  
  predicted_probabilities <- predict(cv_model, test_data, type = "response")
  predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
  
  conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$High_Admissions)
  
  # Accuracy, ROC-AUC, and F1 Score
  cv_accuracy[i] <- mean(predicted_classes == test_data$High_Admissions)
  roc_curve <- roc(test_data$High_Admissions, predicted_probabilities)
  cv_auc[i] <- auc(roc_curve)
  cv_f1[i] <- calculate_f1(conf_matrix)
}

# Summarize cross-validation results
results_df <- data.frame(
  Fold = 1:k,
  Accuracy = round(cv_accuracy, 3),
  AUC = round(cv_auc, 3),
  F1_Score = round(cv_f1, 3)
)

# Add mean row
results_df <- rbind(results_df, c("Mean", round(mean(cv_accuracy), 3), round(mean(cv_auc), 3), round(mean(cv_f1), 3)))

# Print the results table
cat("Cross-Validation Results Summary:\n")
kable(results_df, format = "html", caption = "Cross-Validation Metrics by Fold") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

