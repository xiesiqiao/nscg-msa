# Set working directory and load dataset (foreign-born sample from 2017-2018 ACS PUMS)
# setwd('path/to/working/directory')  # Uncomment and specify the path as needed
# data <- read.csv('path/to/fb_sample_2017_2018_acs_pums.csv')  # Replace with actual dataset path

# Load necessary libraries
library(glmnet)

# Data preparation
data <- na.omit(data)  # Remove rows with missing values
data$age2 <- data$AGE^2  # Add squared age term


# Define response variable as the binary indicator (for logistic regression)
Y <- as.factor(data$MET)  # Make sure Y is a factor for binary outcomes

# Convert categorical predictors to dummy variables, excluding weight and ID variables
exclude_vars <- c("PERWT", "SERIALNO", "RELP")
predictors <- data[, !(names(data) %in% exclude_vars)]
X <- model.matrix(~ . - 1, data = predictors)  # '-1' removes the intercept

# Set up the Lasso logistic regression model
lasso_logit_model <- glmnet(x = X, y = Y, weights = PERWT, alpha = 1, family = "binomial")

# Cross-validation to select the best lambda for the logistic Lasso model
cv_lasso_logit <- cv.glmnet(x = X, y = Y, weights = PERWT, alpha = 1, family = "binomial")
best_lambda_logit <- cv_lasso_logit$lambda.min

# Fit the final Lasso logistic model with the selected lambda
final_logit_model <- glmnet(x = X, y = Y, weights = PERWT, alpha = 1, family = "binomial", lambda = best_lambda_logit)

# Extract coefficients for the logistic model
logit_coefficients <- coef(final_logit_model, s = best_lambda_logit)
logit_coeff_matrix <- as.matrix(logit_coefficients)
logit_coeff_df <- data.frame(Variable = rownames(logit_coeff_matrix), Coefficient = logit_coeff_matrix[, 1])

# Remove intercept and filter out zero coefficients
logit_coeff_df <- logit_coeff_df[-1, ]
logit_coeff_df <- logit_coeff_df[logit_coeff_df$Coefficient != 0, ]

# Save selected variables and their coefficients
write.csv(logit_coeff_df, 'lasso_logit_diag.csv', row.names = FALSE)

# Display the selected variables and their coefficients
print(logit_coeff_df)
