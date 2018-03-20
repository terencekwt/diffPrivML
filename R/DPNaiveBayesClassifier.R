#' @include noiseFunctions.R
NULL

#'
#'Implementation of DPNaiveBayesClassifier (S3 Class).
#'
#'This is a naive bayes classifier with output perturbation implemented
#'as the differential private procedure. The algorithm uses laplace noise.
#'
#'@author Terence Tam
#'
#'@param y a named vector of outcome labels
#'@param x a data frame with named predictor columns
#'@param epsilon privacy budget in epsilon-differential private procedure
#'
#'@return a classifier object trained with naive bayes 4 attributes.
#' 1) likelihoods : a table that can be used to calculate P(X_i | C_j)
#' 2) prioProb : the prior distribution P(C_j)
#' 3) classLabels : the list of distinct labels from Y
#' 4) epsilon : privacy budget
#'
#'@export
#'
#'@examples
#'data(iris)
#'y <- iris[, 5]
#'x <- iris[, 1:4]
#'naiveBayesDP <- DPNaiveBayesClassifier(y, x, epsilon = 1.0)
#'
DPNaiveBayesClassifier <- function(y, x, epsilon = NULL){

  # validations before we proceed
  if(nrow(x) != length(y)){
    stop("ERROR: The predictors x and class labels y have unequal lengths.")
  }

  # training data
  trainingSetX <- x
  trainingSetY <- y
  classLabels <- names(table(trainingSetY))

  #P(C_j)
  priorProb <- laplace(as.vector(table(trainingSetY)), 1, 1)/ length(trainingSetY)

  # statistics that is needed to calculate P(a_i|C_j)
  likelihoods <-
    sapply(names(trainingSetX), function(featureName) {
      feature <- trainingSetX[[featureName]]
      if (is.numeric(feature)) {
        # mean and sd for all P(a_i|C_j) calculating purposes
        sensitivity <- (max(feature) - min(feature)) / (length(feature) + 1)
        statsTable <- rbind(tapply(feature, trainingSetY,
                                   function(x) {laplace(mean(x), sensitivity, epsilon)}),
                            tapply(feature, trainingSetY,
                                   function(x) {laplace(stats::sd(x), sensitivity, epsilon)}))
        rownames(statsTable) <- c("mean", "sd")
        names(dimnames(statsTable)) <- c(featureName, "")
        as.table(statsTable)
      } else {
        # it is categorical attribute, therefore just count cardinalities
        # and then divide by the # of samples belonging to that class
        statsTable <- cbind(feature, trainingSetY)
        statsTable <- apply(table(statsTable), 2,
                            function(column) { column / length(column)})
      }
    }, simplify = FALSE)

  self <- list(likelihoods = likelihoods, priorProb = priorProb,
       classLabels = classLabels, epsilon = epsilon)

  class(self) <- append("DPNaiveBayesClassifier", class(self))
  return(self)
}

#'S3 method for DPNaiveBayesClassifier's summary.
#'
#'@author Terence Tam
#'
#'@param object the classifier object for this predict method
#'@param ... not applicable for this class
#'
#'@export
#'
#'@examples
#'data(iris)
#'y <- iris[, 5]
#'x <- iris[, 1:4]
#'naiveBayesDP <- DPNaiveBayesClassifier(y, x)
#'summary(naiveBayesDP)
#'
summary.DPNaiveBayesClassifier <- function(object, ...) {
  cat("## Class labels:", object$classLabels, "\n\n")
  cat("## Privacy budget:", object$epsilon, "\n\n")
  cat("## Prior probabilites:", object$priorProb, "\n\n")
  cat("## Likelihoods tabulation:", "\n")
  for (item in object$likelihoods) {
    cat("## ")
    print(item)
  }
}

#'S3 method for predict
#'
#'@author Terence Tam
#'
#'@param object the classifier object for this predict method
#'@param testSetX a data frame with named predictor columns
#'@param ... not applicable for this class
#'
#'@importFrom stats dnorm
#'@import caret
#'
#'@export
#'
#'@examples
#'library(caret)
#'data(iris)
#'y <- iris[, 5]
#'x <- iris[, 1:4]
#'trainingIndices <-createDataPartition(y, p = 0.70, list = FALSE)
#'trainingSetX <- x[trainingIndices, ]
#'trainingSetY <- y[trainingIndices]
#'testSetX <- x[-trainingIndices, ]
#'testSetY <- y[-trainingIndices]
#'naiveBayesDP <- DPNaiveBayesClassifier(trainingSetY, trainingSetX)
#'predictions <- predict(naiveBayesDP, testSetX)
#'accuracy <- sum(predictions == testSetY) / length(testSetY)
#'
predict.DPNaiveBayesClassifier <- function(object, testSetX = NULL, ...){

  predictOneData <- function(data){
    likelihoods <- object$likelihoods
    pnorms <- sapply(object$classLabels, function(y) {
      prod(sapply(names(testSetX), function(x){
        if (is.numeric(data[[x]])) {
          stats::dnorm(data[[x]], mean = likelihoods[[x]]['mean', y],
                       sd = likelihoods[[x]]['sd', y])
        } else {
          # categorical variable
          likelihoods[[x]][y, data[[x]]]
        }
      }))
    })
    posteriorProb <- object$priorProb * pnorms
    prediction <- object$classLabels[which.max(posteriorProb)]
    prediction
  }
  apply(testSetX, 1, predictOneData)
}

