#'This is a factory generator for a RandomDecisionTree R6 class object.
#'A RandomDeciscionTree, RDT, is a type of decision tree where the partitioning
#'at each branch is done randomly, unlike other criterion based CART trees.
#'
#'@importFrom R6 R6Class
#'@author Terence Tam
#'
RandomDecisionTree <- R6::R6Class("RandomDecisionTree",
  public = list(
    initialize = function(predictors, levelsY, levelsX, height) {

      private$levelsY = levelsY
      private$levelsX = levelsX
      private$height = height

      #at each node, we picked a new predictor at random to the RDT, but
      #don't pick any that is already used in earlier nodes
      private$chosen <- predictors[sample(length(predictors),1)]

      if(length(levelsX) - length(predictors) < private$height) {
        remaining <- predictors[predictors != private$chosen]
        chosenLevels <- private$levelsX[[private$chosen]]

        # for each level of the chosen predictor, we would spawn a new child
        # branch node in the tree
        private$children <- c()
        i <- 1
        while(i <= length(chosenLevels)) {
          private$children<-c(private$children,
                              RandomDecisionTree$new(predictors = remaining,
                                                     levelsY = private$levelsY,
                                                     levelsX = private$levelsX,
                                                     height = private$height))
          i=i+1
        }
        names(private$children) <- chosenLevels
      } else {
        # this is a leaf
        private$children <- NULL
      }
      private$counter <- rep(0, length(private$levelsY))
      names(private$counter) <- private$levelsY
    },
    #
    # train the tree for each data point
    #
    updateStats = function(row, label){
      level <- row[[private$chosen]]
      if (is.null(private$children)){
        private$counter[[label]] <- private$counter[[label]] + 1
      } else {
        if(is.na(level)) {
          #data has na value, just pick one at random to go down the tree
          level <- private$levelsX[[private$chosen]][[1]]
        }
        child <- private$children[[level]]
        child$updateStats(row=row, label=label)
      }
    },
    printTree = function(){
      if(is.null(private$children)){
        private$chosen
      } else {
        paste(private$chosen, "(", private$children[[1]]$printTree(), private$children[[2]]$printTree(), ")")
      }
    },
    getCounter = function(row){
      level <- row[[private$chosen]]
      if (is.null(private$children)){
        private$counter
      } else {
        if(is.na(level)) {
          #data has na value, just pick one at random to go down the tree
          level <- private$levelsX[[private$chosen]][[1]]
        }
        child <- private$children[[level]]
        child$getCounter(row=row)
      }
    },
    getChildren = function(row){
      private$children
    }
  ),
  private = list(
    chosen = NA,
    children = NULL,
    counter = NULL,
    levelsY = NULL,
    levelsX = NULL,
    height = NA
  ))

#'
#'Implementation of DPRandomDecisionTreeClassifier. An S3 class.
#'
#'@author Terence Tam
#'
#'@param Y a named vector of outcome labels
#'@param X a data frame with named predictor columns
#'@param epsilon privacy budget in epsilon-differential private procedure
#'@param mechanism function for DF (i.e. Laplace, Gaussian)
#'@param numTrees # of trees to grow in the ensemble, default is 5
#'@param height the maxinum height of each decision tree
#'
#'@export
#'
DPRandomDecisionTreeClassifier <- function(Y, X, epsilon = NULL, mechanism = NULL,
                                           numTrees = 5, height = ncol(X)/2){

  #for each predictor and also the resonse, keep track of the cardinalities
  #so we can use them for branching in decision tree
  classLabels <- levels(Y)
  levelsX <- lapply(names(X), function(predictor) { levels(X[[predictor]])})
  names(levelsX) <- names(X)

  #grow trees
  trees <- c()
  i <- 1
  while(i <= numTrees) {
    tree <- RandomDecisionTree$new(predictors=names(X), levelsY = classLabels,
                                   levelsX = levelsX, height = height)
    tree$printTree()
    #train the tree
    for(row in 1:nrow(X)){
      tree$updateStats(row=X[row,], label=Y[row])
    }
    #tree$printTree()
    trees<-c(trees, tree)
    i=i+1
  }
  self <- list(trees = trees, classLabels = classLabels, epsilon = epsilon)

  class(self) <- append("DPRandomDecisionTreeClassifier", class(self))
  return(self)
}

#TODO should be in its own class later, this is for just a makeshift laplace
laplace <- function(originalData, sensitivity, epsilon) {
  scaleFactor <- sensitivity / epsilon
  noise <- -1
    if (is.null(epsilon)){
      noise <- 0
    } else {
      noise <- stats::rexp(1, rate = 1/scaleFactor) -
        stats::rexp(1, rate = 1/scaleFactor)
    }
  originalData + noise
}

#'S3 method for predict
#'
#'@author Terence Tam
#'
#'@param object the classifier object for this predict method
#'@param testSetX a data frame with named predictor columns
#'@param ... not applicable for this class
#'
#'@export
#'
predict.DPRandomDecisionTreeClassifier <- function(object, testSetX = NULL, ...){

  trees <- object$trees

  predictOneData <- function(data){
    countsByClass <- lapply(trees, function(tree){sapply(tree$getCounter(data), function(x){laplace(x,1,NULL)})})
    countsByClass <- Reduce("+", countsByClass)
    probabilities <- countsByClass / sum(countsByClass)
    prediction <- object$classLabels[which.max(probabilities)]
  }

  apply(testSetX, 1, predictOneData)
}


test <- function(){

  #Toy example, remove later

  A <- c('1','0','1','0','1','1','1','0')
  B <- c('0','0','0','1','1','1','0','0')
  C <- c('1','1','0','0','1','1','1','1')
  D <- c('0','0','0','0','0','0','0','1')
  E <- c('0','0','0','1','0','1','1','0')
  Y <-c('T','F','T','F','T','F','T','T')
  X <- as.data.frame(cbind(A,B,C,D,E))

  library(caret)
  data(BreastCancer)

  fullY <- BreastCancer[, 11]
  fullX <- BreastCancer[, 2:10]

  trainingIndices <-createDataPartition(fullY, p = 0.70, list = FALSE)
  trainingSetX <- fullX[trainingIndices, ]
  trainingSetY <- fullY[trainingIndices]
  testSetX <- fullX[-trainingIndices, ]
  testSetY <- fullY[-trainingIndices]

  rdtDP <- DPRandomDecisionTreeClassifier(Y = trainingSetY, X = trainingSetX,
                                          height = 3, numTrees = 5)

  predictions <- predict(rdtDP, testSetX)
  accuracy <- sum(predictions == testSetY) / length(testSetY)

}




