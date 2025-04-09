library(testthat)
library(e2tree)
library(randomForest)

test_that("ePredTree works correctly for classification", {
  set.seed(123)
  
  data(iris)
  train_idx <- sample(seq_len(nrow(iris)), size = 0.75 * nrow(iris))
  training <- iris[train_idx, ]
  validation <- iris[-train_idx, ]
  
  ensemble <- randomForest(Species ~ ., data=training, proximity=TRUE)
  D <- createDisMatrix(ensemble, data=training, label="Species", parallel = list(active=FALSE, no_cores = 1))
  setting <- list(impTotal=0.1, maxDec=0.01, n=2, level=5)
  tree <- e2tree(Species ~ ., training, D, ensemble, setting)
  
  pred <- ePredTree(tree, validation, target="setosa")
  
  expect_s3_class(pred, "data.frame")
  expect_true(all(c("fit", "accuracy", "score") %in% names(pred)))
  expect_equal(nrow(pred), nrow(validation))
  expect_type(pred$fit, "character")
})

test_that("ePredTree works correctly for regression", {
  set.seed(123)
  
  data(mtcars)
  train_idx <- sample(seq_len(nrow(mtcars)), size = 0.75 * nrow(mtcars))
  training <- mtcars[train_idx, ]
  validation <- mtcars[-train_idx, ]
  
  ensemble <- randomForest(mpg ~ ., data=training, proximity=TRUE)
  D <- createDisMatrix(ensemble, data=training, label="mpg", parallel = list(active=FALSE, no_cores = 1))
  setting <- list(impTotal=0.1, maxDec=1e-6, n=2, level=5)
  tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
  
  pred <- ePredTree(tree, validation)
  
  expect_s3_class(pred, "data.frame")
  expect_true(all(c("fit", "accuracy", "score") %in% names(pred)))
  expect_equal(nrow(pred), nrow(validation))
  expect_type(pred$fit, "double") 
  expect_true(all(is.na(pred$accuracy)))
  expect_true(all(is.na(pred$score)))
})
