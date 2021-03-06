% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DPRandomDecisionTreeClassifier.R
\name{predict.DPRandomDecisionTreeClassifier}
\alias{predict.DPRandomDecisionTreeClassifier}
\title{S3 method for DPRandomDecisionTreeClassifier's predict}
\usage{
\method{predict}{DPRandomDecisionTreeClassifier}(object, testSetX = NULL, ...)
}
\arguments{
\item{object}{the classifier object for this predict method}

\item{testSetX}{a data frame with named predictor columns}

\item{...}{not applicable for this class}
}
\description{
S3 method for DPRandomDecisionTreeClassifier's predict
}
\examples{
library(caret)
data(BreastCancer)
Y <- BreastCancer[, 11]
X <- BreastCancer[, 2:10]
trainingIndices <-createDataPartition(Y, p = 0.70, list = FALSE)
trainingSetX <- X[trainingIndices, ]
trainingSetY <- Y[trainingIndices]
testSetX <- X[-trainingIndices, ]
testSetY <- Y[-trainingIndices]
rdtDP <- DPRandomDecisionTreeClassifier(Y = trainingSetY, X = trainingSetX,
height = 4, numTrees = 5, epsilon = 0.8)
predictions <- predict(rdtDP, testSetX)
accuracy <- sum(predictions == testSetY) / length(testSetY)

}
\author{
Terence Tam
}
