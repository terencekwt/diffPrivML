library(diffPrivML)
context("Logistic Regression Classifier")


test_that("error is thrown for bad input", {
  x1 <- c(1,2,3,1,2,3,1,2)
  x2 <- c(1,2,3,1,2,3,1,2)
  X <- as.matrix(cbind(x1, x2))

  #non-regularized model
  Y <-as.factor(c('T','F','T','F','T','F','T','T'))

  expect_error(DPLogisticRegressionClassifier(y = Y, x = X, lambda = 0 ),
               "ERROR: Bad user input. A positive number is required for lambda",
               fixed = TRUE)

})

