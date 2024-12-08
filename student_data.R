if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


my_data <- read.csv("Student_Performance.csv")
head(my_data)
str(my_data)

# Calculate summary statistics for Previous.Scores
mean_PreviousScores <- mean(my_data$Previous.Scores, na.rm = TRUE)
median_PreviousScores <- median(my_data$Previous.Scores, na.rm = TRUE)
sd_PreviousScores <- sd(my_data$Previous.Scores, na.rm = TRUE)
min_PreviousScores <- min(my_data$Previous.Scores, na.rm = TRUE)
max_PreviousScores <- max(my_data$Previous.Scores, na.rm = TRUE)

# Create a data frame to store the results
summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum"),
  Value = c(mean_PreviousScores, median_PreviousScores, sd_PreviousScores, min_PreviousScores, max_PreviousScores)
)

# Print the table
print(summary_table)


# Calculate the summary statistics for all the variables

# Select numerical variables
numerical_vars <- my_data[, sapply(my_data, is.numeric)]

# Calculate statistics
stats <- data.frame(
  Variable = colnames(numerical_vars),
  Min = sapply(numerical_vars, min),
  Max = sapply(numerical_vars, max),
  Mean = sapply(numerical_vars, mean),
  Median = sapply(numerical_vars, median),
  StdDev = sapply(numerical_vars, sd)
)

# Display the statistics table
library(knitr)  # For better table formatting
kable(stats, caption = "Summary Statistics for Numerical Variables")



# Create a histogram for Previous.Scores
hist(my_data$Previous.Scores,
     main = "Histogram of Previous.Scores",
     xlab = "Previous.Scores",
     col = "skyblue",
     border = "black")

# Create a boxplot for Previous.Scores
boxplot(my_data$Previous.Scores,
        main = "Boxplot of Previous.Scores",
        ylab = "Previous.Scores",
        col = "lightgreen",
        horizontal = TRUE)

# Create a bar plot for Sleep.Hours
barplot(table(my_data$Sleep.Hours),
        main = "Distribution of Sleep.Hours",
        xlab = "Sleep Hours",
        ylab = "Frequency",
        col = "skyblue",
        border = "black")

# Calculate Pearson correlation between 'Previous.Scores' and 'Performance.Index'
correlation <- cor(my_data$Previous.Scores, my_data$Performance.Index, method = "pearson", use = "complete.obs")

# Print the result
cat("Pearson Correlation Coefficient between Previous.Scores and Performance.Index:", correlation)



# Extract numerical variables
numerical_vars <- my_data[, sapply(my_data, is.numeric)]

# Calculate the correlation matrix for numerical variables
correlation_matrix <- cor(numerical_vars)

# Print the correlation matrix
print(correlation_matrix)


# Create a scatter plot for Previous.Scores vs Performance.Index
plot(my_data$Previous.Scores, my_data$Performance.Index,
     main = "Scatter Plot of Previous.Scores vs Performance.Index",
     xlab = "Previous.Scores",
     ylab = "Performance.Index",
     col = "blue",    # Color of the points
     pch = 16)        # Shape of the points

# Add a regression line (trend line)
abline(lm(Performance.Index ~ Previous.Scores, data = my_data), col = "red")  # Red trend line

# Fit a linear regression model predicting Performance.Index from Previous.Scores
model <- lm(Performance.Index ~ Previous.Scores, data = my_data)

# Display the summary of the model
summary(model)

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)


# Fit a linear regression model predicting Performance.Index from Previous.Scores and Hours.Studied
model <- lm(Performance.Index ~ Previous.Scores + Hours.Studied, data = my_data)

# Display the summary of the updated model
summary(model)

# Diagnostic plots for the updated model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)



# ======================= Principal Component Analysis =======================

# Convert non-numeric variables to numeric if needed
# Example: If 'Sample.Question.Papers.Practiced' is a factor or character
my_data$Sample.Question.Papers.Practiced <- as.numeric(my_data$Sample.Question.Papers.Practiced)

# Select numerical variables for PCA
numerical_vars <- my_data[, sapply(my_data, is.numeric)]

# Step 2: Standardize the data
numerical_vars_scaled <- scale(numerical_vars)

# Step 3: Perform PCA
pca_result <- prcomp(numerical_vars_scaled, center = TRUE, scale. = TRUE)

# Step 4: Scree Plot (Explained Variance)
explained_variance <- summary(pca_result)$importance[2, ]  # Proportion of variance

# Plot the explained variance (Scree Plot)
plot(explained_variance, type = "b", 
     main = "Scree Plot", 
     xlab = "Principal Components", 
     ylab = "Proportion of Variance Explained",
     pch = 19, col = "blue", lwd = 2)

# Step 5: Visualize the PCA results with a Biplot
biplot(pca_result, scale = 0, 
       main = "PCA Biplot (Scaled)", 
       col = c("black", "red"))













