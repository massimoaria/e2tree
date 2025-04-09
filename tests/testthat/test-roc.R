library(testthat)
library(ggplot2)  

test_that("roc() returns expected output structure and values", {
  set.seed(123)
  
  # data
  response <- rep(c("yes", "no"), each = 50)
  scores <- c(runif(50, 0.6, 1), runif(50, 0, 0.4))  
  target <- "yes"
  
  # function
  result <- roc(response, scores, target)
  
  # structure
  expect_type(result, "list")
  expect_named(result, c("roc_data", "auc"))
  
  # check on dataframe
  expect_s3_class(result$roc_data, "data.frame")
  expect_true(all(c("TPR", "FPR", "labels") %in% names(result$roc_data)))
  expect_equal(nrow(result$roc_data), length(response))
  
  # AUC 
  expect_gt(result$auc, 0.9)
})

test_that("roc() handles minimal input correctly", {
  response <- c("yes", "no")
  scores <- c(0.9, 0.1)
  target <- "yes"
  
  result <- roc(response, scores, target)
  
  expect_type(result, "list")
  expect_true(result$auc == 1 || result$auc == 0.5)  
})
