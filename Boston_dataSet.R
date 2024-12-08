if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dev.new()

# Reading the data
my_data <- read.csv("Boston.csv")
str(my_data)
head(my_data)

# Number of observations and variables
dim(my_data)
# Number of rows (observations)
nrow(my_data)

# Number of columns (variables)
ncol(my_data)

# Calculate summary statistics for 'indus'
mean_indus <- mean(my_data$indus, na.rm = TRUE)
median_indus <- median(my_data$indus, na.rm = TRUE)
sd_indus <- sd(my_data$indus, na.rm = TRUE)
min_indus <- min(my_data$indus, na.rm = TRUE)
max_indus <- max(my_data$indus, na.rm = TRUE)


# Create a data frame to store the results
summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum"),
  Value = c(mean_indus, median_indus, sd_indus, min_indus, max_indus)
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






# Create a histogram
hist(my_data$indus,
     main = "Histogram of indus",
     xlab = "indus",
     col = "skyblue",
     border = "black")

# Create a boxplot
boxplot(my_data$indus,
        main = "Boxplot of indus",
        ylab = "indus",
        col = "lightgreen",
        horizontal = TRUE)

# Create a bar plot for the 'chas' variable
barplot(table(my_data$chas),
        main = "Distribution of CHAS",
        xlab = "CHAS",
        ylab = "Frequency",
        col = c("skyblue", "lightgreen"),
        border = "black")


# Calculate Pearson correlation between 'age' and 'medv'
correlation <- cor(my_data$rm, my_data$medv, method = "pearson", use = "complete.obs")

# Print the result
cat("Pearson Correlation Coefficient between age and medv:", correlation)

# Creating a correlation matrix for all the variables

# Extract numerical variables
numerical_vars <- my_data[, sapply(my_data, is.numeric)]

# Calculate the correlation matrix for numerical variables
correlation_matrix <- cor(numerical_vars)

# Print the correlation matrix
print(correlation_matrix)


# Create a scatter plot for 'rm' vs 'medv'
plot(my_data$rm, my_data$medv,
     main = "Scatter Plot of RM vs MEDV",
     xlab = "Average Number of Rooms (RM)",
     ylab = "Median Value (MEDV)",
     col = "blue",    # Color of the points
     pch = 16)        # Shape of the points

# Add a regression line (trend line)
abline(lm(medv ~ rm, data = my_data), col = "red")  # Red trend line

# ================ linear regression model ==================

# Fit a linear regression model predicting 'Rings' from 'Diameter' and 'Whole.weight'
model <- lm(medv ~ rm + lstat, data = my_data)

# Display the summary of the model
summary(model)

# ================ Residuals plots ==================

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)

# ================ Fit one more variable variables ==================

model <- lm(medv ~ rm + lstat + ptratio, data = my_data)
# Display the summary of the model
summary(model)
# ================ Residuals plots ==================

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)

# ================ Fit only significant variables ==================
# Convert 'chas' to a factor since it's categorical
my_data$chas <- as.factor(my_data$chas)

# Fit the linear regression model, excluding 'indus', 'age', and 'X'
model <- lm(medv ~ . - indus - age - X, data = my_data)

# Display the summary of the model
summary(model)

# Residual plots to check assumptions
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)


# ================ Principal Component Analysis ==================

# Step 1: Select numerical variables for PCA
numerical_vars <- my_data[, c("crim", "zn", "indus", "nox", "rm", "age", 
                              "dis", "rad", "tax", "ptratio", "black", "lstat")]

# Step 2: Standardize the data
numerical_vars_scaled <- scale(numerical_vars)

# Step 3: Perform PCA
pca_result <- prcomp(numerical_vars_scaled, center = TRUE, scale. = TRUE)

# Step 4: Scree Plot (Explained Variance)
# Variance explained by each component
explained_variance <- summary(pca_result)$importance[2,]

# Plot the explained variance
plot(explained_variance, type = "b", 
     main = "Scree Plot", 
     xlab = "Principal Components", 
     ylab = "Proportion of Variance Explained",
     pch = 19, col = "blue", lwd = 2)

# Optional: Add a line for cumulative variance
cumulative_variance <- cumsum(explained_variance)
lines(cumulative_variance, type = "b", col = "red", lwd = 2)

# Step 5: Visualize the PCA results with a Biplot
biplot(pca_result, scale = 0, 
       main = "PCA Biplot (Scaled)", 
       col = c("black", "red"))




















