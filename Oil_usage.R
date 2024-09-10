
               # Project:1

library(readxl)
library(ggplot2)
library(caTools)

#loading dataset
oil_data <- read_excel("C:/Users/divya/Desktop/conestoga/predictive analytics/sem 2/Advance data modelling for Analytics/Group project/Oil dataset.xlsx")

# checking missing value
missing_values <-  colSums(is.na(oil_data))
missing_values

set.seed(123)
split <- sample.split(oil_data$`Oil Usage`, SplitRatio = 0.7)
train_data <- subset(oil_data, split == TRUE)
test_data <- subset(oil_data, split == FALSE)

# Create a histogram of Oil Usage to analyse the data
hist(oil_data$`Oil Usage`,
     main = "Histogram of Oil Usage",
     xlab = "Oil Usage",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

# Create a box plot to analyse categorical variable Home index
boxplot(`Oil Usage` ~ `Home Index`, 
        data = oil_data,
        main = "Box Plot of Oil Usage vs Home Index",
        xlab = "Home Index",
        ylab = "Oil Usage",
        col = "lightgreen",
        border = "black")

# create a box plot to analyse categorical variable (Number of people)
boxplot(`Oil Usage` ~ `Number People`, 
        data = oil_data,
        main = "Box Plot of Oil Usage vs Number of people",
        xlab = "Number of people",
        ylab = "Oil Usage",
        col = "pink",
        border = "black")


#creating linear model
model <- lm(`Oil Usage` ~ `Degree Days` + `Home Index` + `Number People`, data = oil_data)
summary_result <- summary(model)
summary_result

# extract the summary components
coe_estimate <- summary_result$coefficients[, "Estimate"]
coe_estimate
standard_errors <- summary_result$coefficients[, "Std. Error"]
standard_errors
t_values <- summary_result$coefficients[, "t value"]
t_values
p_values <- summary_result$coefficients[, "Pr(>|t|)"]
p_values
r_squared <- summary_result$r.squared
r_squared
adj_r_squared <- summary_result$adj.r.squared
adj_r_squared
residual_std_error <- summary_result$sigma
residual_std_error
f_statistic <- summary_result$fstatistic
f_statistic


# Create a residual model
residuals <- model$residuals
summary(residuals)

# Create a histogram of residual
hist(residuals,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")


# Calculate the fitted values and residuals
fitted_values <- fitted(model)
residuals <- resid(model)

# Create a scatter plot of residual vs fitted plot
plot(fitted_values, residuals,
     main = "Residuals vs. Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, # Plotting character
     col = "blue")

# Add a horizontal line at y=0 for reference
abline(h = 0, col = "red", lty = 2)


# determining significance level
coefficient_summary <- summary_result$coefficients
p_values <- summary_result$coefficients[, 4]
significant_vars <- coefficient_summary[p_values < 0.05, ]

print("Statistically significant variables (p < 0.05):")
print(significant_vars)

interpret_coefficients <- function(coefficients_summary) {
  for (i in 1:nrow(coefficients_summary)) {
    var_name <- rownames(coefficients_summary)[i]
    estimate <- coefficients_summary[i, "Estimate"]
    std_error <- coefficients_summary[i, "Std. Error"]
    p_value <- coefficients_summary[i, "Pr(>|t|)"]
    
    significance <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", " ")))
    
    cat(sprintf("%s: Estimate = %.5f, Std. Error = %.5f, p-value = %.5f %s\n",
                var_name, estimate, std_error, p_value, significance))
  }
}

# Interpret the coefficients
cat("Coefficients Interpretation:\n")
interpret_coefficients(coefficient_summary)

# Model evaluation on test data and making prediction
test_predictions <- predict(model, newdata = test_data)
test_residuals <- test_data$`Oil Usage` - test_predictions

# Calculate and print the Mean Squared Error (MSE) and Root Mean Squared Error (RMSE)
mse <- mean(test_residuals^2)
rmse <- sqrt(mse)
cat("Mean Squared Error (MSE) on test data:", mse, "\n")
cat("Root Mean Squared Error (RMSE) on test data:", rmse, "\n")

summary(test_predictions)


# Additionally, you can plot the predicted values against the actual values to visually assess the model's performance
plot(test_data$`Oil Usage`, test_predictions, xlab ="Actual oil use" , ylab ="Predicted oil use" , main = "Predicted vs Actual")
abline(0, 1, col = "red")







