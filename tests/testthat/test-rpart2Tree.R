library(testthat)
library(e2tree)
library(randomForest)

test_that("rpart2Tree works correctly with valid inputs", {
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
  
  # Run rpart2Tree
  rpart_obj <- rpart2Tree(fit, ensemble)
  
  # Tests
  expect_type(rpart_obj, "list")  # Should return a list
  expect_true("frame" %in% names(rpart_obj))
  expect_true("where" %in% names(rpart_obj))
  expect_true("variable.importance" %in% names(rpart_obj))
})

test_that("rpart2Tree handles incorrect input types", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = FALSE)
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  fit <- e2tree(Species ~ ., training, D, ensemble, setting)

  # Test incorrect inputs
  expect_error(rpart2Tree(NULL, ensemble), 
               "Error: 'fit' must be an 'e2tree' object.")
  
  expect_error(rpart2Tree(fit, NULL), 
               "Error: 'ensemble' must be a trained 'randomForest' model.")
  
  expect_error(rpart2Tree(fit, list()), 
               "Error: 'ensemble' must be a trained 'randomForest' model.")
})

test_that("rpart2Tree handles invalid ensemble type", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = FALSE)
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
  fit <- e2tree(Species ~ ., training, D, ensemble, setting)
  ensemble$type <- "unknown_type"  # Modify to invalid type
  expect_error(rpart2Tree(fit, ensemble), "Error: 'type' in ensemble object must be either 'classification' or 'regression'.")


})

