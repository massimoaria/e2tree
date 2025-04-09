library(testthat)
library(e2tree)
library(randomForest)

test_that("e2tree works correctly for classification", {
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
  
  # Run e2tree
  tree <- e2tree(Species ~ ., training, D, ensemble, setting)
  
  # Tests
  expect_type(tree, "list")  # Must return a list
  expect_true("tree" %in% names(tree))  # Must contain a tree object
  expect_true(is.data.frame(tree$tree))  # The tree should be a data frame
})

test_that("e2tree works correctly for regression", {
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
  setting=list(impTotal=0.1, maxDec=(1*10^-6), n=2, level=5)
  
  # Run e2tree
  tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
  
  # Tests
  expect_type(tree, "list")  # Must return a list
  expect_true("tree" %in% names(tree))  # Must contain a tree object
  expect_true(is.data.frame(tree$tree))  # The tree should be a data frame
})

test_that("e2tree handles incorrect input types", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = list(active=FALSE, no_cores = 1))
  setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)

  # Test incorrect inputs
  expect_error(e2tree(NULL, training, D, ensemble, setting), 
               "Error: 'formula' must be a valid formula object.")
  
  expect_error(e2tree(Species ~ ., NULL, D, ensemble, setting), 
               "Error: 'data' must be a non-empty data frame.")
  
  expect_error(e2tree(Species ~ ., training, NULL, ensemble, setting), 
               "Error: 'D' must be a square dissimilarity matrix.")
  
  expect_error(e2tree(Species ~ ., training, D, NULL, setting), 
               "Error: 'ensemble' must be a trained 'randomForest' model.")
  
  expect_error(e2tree(Species ~ ., training, D, ensemble, NULL), 
               "Error: 'setting' must be a list with keys: 'impTotal', 'maxDec', 'n', and 'level'.")
  
  ensemble$type <- "unknown_type"  # Modify to invalid type
  expect_error(e2tree(Species ~ ., training, D, ensemble, setting), 
               "Error: 'type' in ensemble object must be either 'classification' or 'regression'.")
})

test_that("e2tree handles incorrect settings", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = list(active=FALSE, no_cores = 1))

  # Incorrect settings
  bad_setting1 <- list(impTotal = -0.1, maxDec = 0.01, n = 2, level = 5)
  bad_setting2 <- list(impTotal = "high", maxDec = 0.01, n = 2, level = 5)
  
  expect_error(e2tree(Species ~ ., training, D, ensemble, bad_setting1), 
               "Error: All values in 'setting' must be positive numeric values.")
  
  expect_error(e2tree(Species ~ ., training, D, ensemble, bad_setting2), 
               "Error: All values in 'setting' must be positive numeric values.")
})

