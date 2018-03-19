library(diffPrivML)
context("Random Decision Tree Classifier")

test_that("the dimensions of the output are correct", {
  x1 <- c('1','0','0','1')
  x2 <- c('1','0','0','1')
  Y <-c('T','F','T','F')
  dataset <- as.data.frame(cbind(x1, x2, Y))

  rdtDP <- DPRandomDecisionTreeClassifier(Y = dataset[,3], X = dataset[,1:2], numTrees = 1)
  expect_equal(length(rdtDP$trees), 1)

  rdtDP <- DPRandomDecisionTreeClassifier(Y = dataset[,3], X = dataset[,1:2], numTrees = 2)
  expect_equal(length(rdtDP$trees), 2)

})
