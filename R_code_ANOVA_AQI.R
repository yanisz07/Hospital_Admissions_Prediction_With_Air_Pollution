# Load necessary libraries

#install.packages(dplyr)
#install.packages(ggplot2)
#install.packages(lubridate)
#install.packages(ggpubr)
#install.packages(car)
#install.packages(WRS2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr) # For precise plots
library(car)  # For Levene's Test
library(WRS2)  # For Welch's ANOVA

# Load the dataset
data <- read.csv("Admissions_Pollution_Sorted.csv")

print(data)

# Ensure Date column is in Date format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Define seasons based on meteorological separation
data <- data %>%
  mutate(Season = case_when(
    month(Date) %in% c(12, 1, 2) ~ "Winter",
    month(Date) %in% c(3, 4, 5) ~ "Spring",
    month(Date) %in% c(6, 7, 8) ~ "Summer",
    month(Date) %in% c(9, 10, 11) ~ "Fall",
    TRUE ~ NA_character_
  ))

# Verify the distribution of AQI across seasons
summary(data$Season)
table(data$Season)

# Perform Levene's Test for homogeneity of variances
levene_test_result <- leveneTest(AQI ~ Season, data = data, center = "median")

# Print Levene's Test results
cat("Levene's Test Results:\n")
print(levene_test_result)


# Perform Welch's ANOVA (because Variance is not equal accross all groups from Levene's Test)
welch_anova_result <- oneway.test(AQI ~ Season, data = data, var.equal = FALSE)

# Print Welch ANOVA results
cat("Welch ANOVA Results:\n")
print(welch_anova_result)

anova_p_value <- welch_anova_result$p.value

# Enhanced boxplot with statistical annotations
ggboxplot(data, x = "Season", y = "AQI",
          color = "Season", palette = "jco",
          add = "jitter") +
  annotate("text", x = 2, y = max(data$AQI) + 50, 
           label = paste("Welch ANOVA, p_value =", signif(anova_p_value, 3))) +
  labs(title = "AQI Differences Across Seasons",
       x = "Season",
       y = "Air Quality Index (AQI)") +
  theme_minimal()

