library(diffPrivML)
context("DP Naive Bayes Classifier")

test_that("the dimensions of the output are correct", {
  x1 <- c(1,2,3,1,2,3,1,2)
  x2 <- c(1,2,3,1,2,3,1,2)
  X <- as.data.frame(cbind(x1, x2))
  Y <-c('T','F','T','F','T','F','T','T')
  naiveBayesDP <- DPNaiveBayesClassifier(y = Y, x = X)

  #items belonging to the class objects
  expect_equal(length(naiveBayesDP), 3)

  #likelihood calculation table per attribute in X
  expect_equal(length(naiveBayesDP$likelihoods), 2)

  #prior probability for each class label in Y
  expect_equal(length(naiveBayesDP$priorProb), 2)

  #number of class labels (cardinalities) in Y
  expect_equal(length(naiveBayesDP$classLabels), 2)
})

test_that("error is thrown for bad input", {
  x1 <- c(1,2,3,1,2,3,1,2)
  x2 <- c(1,2,3,1,2,3,1,2)
  X <- as.data.frame(cbind(x1, x2))

  #one extra element
  Y <-c('T','F','T','F','T','F','T','T','F')

  #items belonging to the class objects
  expect_error(DPNaiveBayesClassifier(y = Y, x = X),
               "ERROR: The predictors x and class labels y have unequal lengths.",
               fixed = TRUE)

})

