library(testthat)
library(e2tree)
library(randomForest)

test_that("eComparison works correctly with valid inputs", {
  set.seed(42)
  
  # Prepare data
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  # Train Random Forest
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  
  # Create dissimilarity matrix
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = FALSE)
  
  # Define settings
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  
  # Generate e2tree model
  fit <- e2tree(Species ~ ., training, D, ensemble, setting)
  
  # Run eComparison
  comparison <- eComparison(training, fit, D, graph=FALSE)
  
  # Tests
  expect_type(comparison, "list")  # Should return a list
  expect_true("mantel_test" %in% names(comparison))
})

test_that("eComparison handles incorrect input types", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = FALSE)
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  fit <- e2tree(Species ~ ., training, D, ensemble, setting)

  # Test incorrect inputs
  expect_error(eComparison(NULL, fit, D, graph=FALSE), 
               "Error: 'data' must be a non-empty data frame.")
  
  expect_error(eComparison(training, NULL, D, graph=FALSE), 
               "Error: 'fit' must be an 'e2tree' object.")
  
  expect_error(eComparison(training, fit, NULL, graph=FALSE), 
               "Error: 'D' must be a square dissimilarity matrix.")
  
  expect_error(eComparison(training, fit, matrix(1, 5, 5), graph=FALSE), 
               "Error: The number of rows in 'data' must match the dimensions of 'D'.")
})
