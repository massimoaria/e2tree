library(testthat)
library(e2tree)  # Ensure the package is loaded
library(randomForest)

test_that("createDisMatrix works correctly for classification task", {
  set.seed(42)
  
  # Prepare data
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  # Train Random Forest
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  
  # Compute dissimilarity matrix
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = list(active=FALSE, no_cores = 1))
  
  # Tests
  expect_type(D, "double")  # Should return a numeric matrix
  expect_true(is.matrix(D)) # Must be a matrix
  expect_equal(dim(D), c(nrow(training), nrow(training)))  # Should match input size
  expect_equal(as.numeric(diag(D)), rep(0, nrow(training))) # Diagonal should be 0 (self-similarity)
})

test_that("createDisMatrix works correctly for regression task", {
  set.seed(42)
  
  # Prepare data
  data(mtcars)
  train_idx <- sample(seq_len(nrow(mtcars)), size = 0.75 * nrow(mtcars))
  training <- mtcars[train_idx, ]
  
  # Train Random Forest
  ensemble <- randomForest(mpg ~ ., data = training, ntree = 100, importance = TRUE, proximity = TRUE)
  
  # Compute dissimilarity matrix
  D <- createDisMatrix(ensemble, data = training, label = "mpg", parallel = list(active=FALSE, no_cores = 1))
  
  # Tests
  expect_type(D, "double")
  expect_true(is.matrix(D))
  expect_equal(dim(D), c(nrow(training), nrow(training)))
  expect_equal(as.numeric(diag(D)), rep(0, nrow(training)))
})

test_that("createDisMatrix handles incorrect input types", {
  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)
  
  expect_error(createDisMatrix(NULL, data = training, label = "Species"), 
               "Error: 'ensemble' cannot be NULL")
  expect_error(createDisMatrix(ensemble, data = NULL, label = "Species"), 
               "Error: 'data' must be a valid data frame")
  expect_error(createDisMatrix(ensemble, data = training, label = "InvalidLabel"), 
               "Error: 'label' must be a valid column name in 'data'")
})

test_that("createDisMatrix works with parallelization", {

  set.seed(42)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data = training, importance = TRUE, proximity = TRUE)

  # Compute dissimilarity matrix in parallel
  D <- createDisMatrix(ensemble, data = training, label = "Species", parallel = list(active=TRUE, no_cores = 1))
  
  expect_true(is.matrix(D))
  expect_equal(dim(D), c(nrow(training), nrow(training)))
})
