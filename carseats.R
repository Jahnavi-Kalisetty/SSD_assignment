if (!requireNamespace("ISLR", quietly = TRUE)) install.packages("ISLR")
library(ISLR)
#data(package="ISLR")
my_data<-Carseats
str(my_data)
head(my_data)

# Number of observations and variables
dim(my_data)  # Dimensions of the dataset
nrow(my_data) # Number of rows (observations)
ncol(my_data) # Number of columns (variables)

# Calculate summary statistics for 'Price'
mean_Price <- mean(my_data$Price, na.rm = TRUE)    # Mean of 'Price'
median_Price <- median(my_data$Price, na.rm = TRUE) # Median of 'Price'
sd_Price <- sd(my_data$Price, na.rm = TRUE)        # Standard deviation of 'Price'
min_Price <- min(my_data$Price, na.rm = TRUE)      # Minimum value of 'Price'
max_Price <- max(my_data$Price, na.rm = TRUE)      # Maximum value of 'Price'

# Print the results
mean_Price
median_Price
sd_Price
min_Price
max_Price

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


# Create a histogram for 'Price'
hist(my_data$Price,
     main = "Histogram of Price",
     xlab = "Price",
     col = "skyblue",
     border = "black")

# Create a boxplot for 'Price'
boxplot(my_data$Price,
        main = "Boxplot of Price",
        ylab = "Price",
        col = "lightgreen",
        horizontal = TRUE)

# Create a bar plot for 'Urban'
barplot(table(my_data$Urban),
        main = "Distribution of Urban",
        xlab = "Urban",
        ylab = "Frequency",
        col = c("skyblue", "lightgreen"),
        border = "black")

# Calculate Pearson correlation between 'Price' and 'Sales'
correlation <- cor(my_data$Price, my_data$Sales, method = "pearson", use = "complete.obs")

# Print the result
cat("Pearson Correlation Coefficient between Price and Sales:", correlation)


# Extract numerical variables
numerical_vars <- my_data[, sapply(my_data, is.numeric)]

# Calculate the correlation matrix for numerical variables
correlation_matrix <- cor(numerical_vars)

# Print the correlation matrix
print(correlation_matrix)


# Create a scatter plot for 'Price' and 'Sales'
plot(my_data$Price, my_data$Sales,
     main = "Scatter Plot of Price vs Sales",
     xlab = "Price",
     ylab = "Sales",
     col = "blue",    # Color of the points
     pch = 16)        # Shape of the points

# Add a regression line (trend line)
abline(lm(Sales ~ Price, data = my_data), col = "red")  # Red trend line

# Fit a linear regression model predicting 'Sales' from 'Age' and 'Price'
model <- lm(Sales ~ Age + Price, data = my_data)

# Display the summary of the model
summary(model)


# Convert categorical variables to factors
my_data$ShelveLoc <- as.factor(my_data$ShelveLoc)
my_data$Urban <- as.factor(my_data$Urban)
my_data$US <- as.factor(my_data$US)

# Fit a multiple linear regression model
model <- lm(Sales ~ . - Population - Urban - US - Education, data = my_data)

# Display the summary of the model
summary(model)


# ================ Residuals plots ==================

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)

# ================ Principal Component Analysis ===============

# Step 1: Select numerical variables
numerical_vars <- my_data[, c("Sales", "CompPrice", "Income", "Advertising", 
                              "Population", "Price", "Age", "Education")]

# Step 2: Standardize the data
numerical_vars_scaled <- scale(numerical_vars)

# Step 3: Perform PCA
pca_result <- prcomp(numerical_vars_scaled, center = TRUE, scale. = TRUE)

# Step 4: Explained variance (Scree plot)
explained_variance <- summary(pca_result)$importance[2, ]  # Proportion of variance

# Scree plot
plot(explained_variance, type = "b", main = "Scree Plot", 
     xlab = "Principal Components", ylab = "Proportion of Variance Explained", 
     pch = 19, col = "blue", lwd = 2)

# Step 5: PCA Biplot
biplot(pca_result, scale = 0, main = "PCA Biplot (Scaled)", col = c("black", "red"))












