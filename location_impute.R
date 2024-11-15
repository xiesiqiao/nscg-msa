# Load necessary library
library(nnet)  # For multinomial logistic regression

# Directory paths and file list
data_path <- 'pums'
nscg_path <- 'nscg'
data_files <- list.files(path = data_path, pattern = "*.csv")
print(data_files)

# Loop through each regional dataset
for (file in data_files) {
  gc()  # Garbage collection to manage memory
  print(paste("Processing:", file))
  
  # Load and preprocess the training dataset (ACS PUMS)
  train_data <- read.csv(file.path(data_path, file))  # Read ACS PUMS data by region
  train_data$id <- rownames(train_data)  # Add row identifier
  
  # Load and preprocess the test dataset (NSCG)
  test_data <- read.csv(file.path(nscg_path, file))  # Read NSCG data by region
  
  # Train multinomial logistic regression model
  model <- multinom(
    MET ~ major + occupation + bpl * decade + log(yr.in.us) +
      race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl * occ.broad,
    data = train_data,
    family = "multinomial",
    weights = PERWT,
    na.action = na.exclude,
    MaxNWts = 1000000,
    maxit = 1500
  )
  gc()  # Garbage collection
  
  # Ensure test data is compatible with training data by subsetting relevant fields
  test_data <- subset(test_data, (test_data$occupation %in% train_data$occupation))
  test_data <- subset(test_data, (test_data$decade %in% train_data$decade))
  
  # Predict probabilities for the test data
  test_probs <- data.frame(predict(model, newdata = test_data, type = "probs"))
  names(test_probs) <- paste0('prob.', names(test_probs))  # Rename columns for clarity
  
  # Combine predictions with test data
  results <- cbind(test_data, test_probs)
  gc()  # Garbage collection
  
  # Merge results with additional country-level data if available
  results <- merge(results, d, by = 'edu.country', all.x = TRUE)
  
  # Save predictions to a CSV file
  output_file <- paste0('pred.', file)
  write.csv(results, file = output_file)
  print(paste("Completed:", file))
}
