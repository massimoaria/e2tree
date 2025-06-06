library(testthat)
library(e2tree)
library(randomForest)

test_that("vimp works correctly with valid inputs (classification case)", {
  set.seed(42)
  
  # Prepare data
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  # Train Random Forest
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  
  # Create dissimilarity matrix
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = list(active=FALSE, no_cores = 1))
  
  # Define settings
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  
  # Generate e2tree model
  fit <- e2tree(Species ~ ., training, D, ensemble, setting)
  
  # Run vimp function
  vimp_result <- vimp(fit, training, type = "classification")
  
  # Tests
  expect_type(vimp_result, "list")  # Should return a list
  expect_true("vimp" %in% names(vimp_result))
  expect_true(is.data.frame(vimp_result$vimp))  # vimp should be a data frame
})

test_that("vimp handles incorrect input types (classification case)", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = list(active=FALSE, no_cores = 1))
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  fit <- e2tree(Species ~ ., training, D, ensemble, setting)

  # Test incorrect inputs
  expect_error(vimp(NULL, training, "classification"), 
               "Error: 'fit' must be an 'e2tree' object.")
  
  expect_error(vimp(fit, NULL, "classification"), 
               "Error: 'data' must be a non-empty data frame.")
  
  expect_error(vimp(fit, training, "unknown_type"), 
               "Error: 'type' must be either 'classification' or 'regression'.")
})

test_that("vimp handles missing response variable in data (classification case)", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = list(active=FALSE, no_cores = 1))
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  fit <- e2tree(Species ~ ., training, D, ensemble, setting)

  # Remove response variable from training data
  training_no_species <- training[, -which(names(training) == "Species")]

  expect_error(vimp(fit, training_no_species, "classification"), 
               "Error: The response variable from 'fit' is not found in 'data'.")
})













test_that("vimp works correctly with valid inputs (regression case)", {
  set.seed(42)
  
  # Prepare data
  data(mtcars)
  train_idx <- sample(seq_len(nrow(mtcars)), size = 0.75 * nrow(mtcars))
  training <- mtcars[train_idx, ]
  
  # Train Random Forest
  ensemble <- randomForest(mpg ~ ., data = training, importance = TRUE, proximity = TRUE)
  
  # Create dissimilarity matrix
  D <- createDisMatrix(ensemble, data = training, label = "mpg", parallel = list(active=FALSE, no_cores = 1))
  
  # Define settings
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  
  # Generate e2tree model
  fit <- e2tree(mpg ~ ., training, D, ensemble, setting)
  
  # Run vimp function
  vimp_result <- vimp(fit, training, type = "regression")
  
  # Tests
  expect_type(vimp_result, "list")  # Should return a list
  expect_true("vimp" %in% names(vimp_result))
  expect_true(is.data.frame(vimp_result$vimp))  # vimp should be a data frame
})

test_that("vimp handles incorrect input types (regression case)", {
  set.seed(42)
  
  data(mtcars)
  train_idx <- sample(seq_len(nrow(mtcars)), size = 0.75 * nrow(mtcars))
  training <- mtcars[train_idx, ]
  
  ensemble <- randomForest(mpg ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "mpg", parallel = list(active=FALSE, no_cores = 1))
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  fit <- e2tree(mpg ~ ., training, D, ensemble, setting)
  
  # Test incorrect inputs
  expect_error(vimp(NULL, training, "classification"), 
               "Error: 'fit' must be an 'e2tree' object.")
  
  expect_error(vimp(fit, NULL, "classification"), 
               "Error: 'data' must be a non-empty data frame.")
  
  expect_error(vimp(fit, training, "unknown_type"), 
               "Error: 'type' must be either 'classification' or 'regression'.")
})

test_that("vimp handles missing response variable in data (regression case)", {
  set.seed(42)
  
  data(mtcars)
  train_idx <- sample(seq_len(nrow(mtcars)), size = 0.75 * nrow(mtcars))
  training <- mtcars[train_idx, ]
  
  ensemble <- randomForest(mpg ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "mpg", parallel = list(active=FALSE, no_cores = 1))
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  fit <- e2tree(mpg ~ ., training, D, ensemble, setting)
  
  # Remove response variable from training data
  training_no_mpg <- training[, -which(names(training) == "mpg")]
  
  expect_error(vimp(fit, training_no_mpg, "classification"), 
               "Error: The response variable from 'fit' is not found in 'data'.")
})
