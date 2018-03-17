#TODO should be in its own class later, this is for just a makeshift laplace
laplace <- function(originalData, sensitivity, epsilon) {
  scaleFactor <- sensitivity / epsilon
  noise <- -1
  while(noise < 0){
    if (is.null(epsilon)){
      noise <- 0
    } else {
      noise <- stats::rexp(1, rate = 1/scaleFactor) -
        stats::rexp(1, rate = 1/scaleFactor)
    }
  }
  originalData + noise
}

#'Implementation of DPNaiveBayesClassifier. An S3 class.
#'
#'@param y a named vector of outcome labels
#'@param x a data frame with named predictor columns
#'@param epsilon privacy budget in epsilon-differential private procedure
#'@param mechanism function for DF (i.e. Laplace, Gaussian)
#'
#'@return a classifier
#'
#'
#'@export
#'
#'@examples
#'data(iris)
#'y <- iris[, 5]
#'x <- iris[, 1:4]
#'naiveBayesDP <- DPNaiveBayesClassifier(y, x, epsilon = 1.0)
#'
DPNaiveBayesClassifier <- function(y, x, epsilon = NULL, mechanism = NULL){

  #validations before we proceed
  if(nrow(x) != length(y)){
    stop("ERROR: The predictors x and class labels y have unequal lengths.")
  }

  #training
  trainingSetX <- x
  trainingSetY <- y
  classLabels <- names(table(trainingSetY))

  #P(C_j)
  priorProb <- laplace(as.vector(table(trainingSetY)), 1, 1)/ length(trainingSetY)

  #mean and sd for all P(a_i|C_j) calculating purposes
  likelihoods <-
    sapply(names(trainingSetX), function(featureName) {
      feature <- trainingSetX[[featureName]]
      if (is.numeric(feature)) {
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
       classLabels = classLabels)

  class(self) <- append("DPNaiveBayesClassifier", class(self))
  return(self)
}

#DF procedure (ouput perturbation)

#'S3 method for predict
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
