#'Get random tree
#'
#'@importFrom R6 R6Class
#'
RandomDecisionTree <- R6::R6Class("RandomDecisionTree",
  public = list(
    initialize = function(predictors) {
      private$chosen <- predictors[sample(length(predictors),1)]
      if(length(predictors) > 2) {
        remaining <- predictors[predictors != private$chosen]
        private$children <- c()
        i <- 1
        while(i <= 2) {
          private$children<-c(private$children, RandomDecisionTree$new(predictors=remaining))
          i=i+1
        }
        names(private$children) <- c('0','1')
      } else {
        private$children <- NULL
      }
      private$counter <- c(0,0)
      names(private$counter) <- c('T','F')
    },
    #
    # train the tree for each data point
    #
    updateStats = function(row, label){
      level <- row[[private$chosen]]
      if (is.null(private$children)){
        private$counter[[label]] <- private$counter[[label]] + 1
      } else {
        child <- private$children[[level]]
        child$updateStats(row=row, label=label)
      }
    },
    printTree = function(){
      if(is.null(private$children)){
        paste(private$chosen,'{',private$counter[1],',', private$counter[2],'}')
      } else {
        paste(private$chosen, "(", private$children[['0']]$printTree(), private$children[['1']]$printTree(), ")")
      }
    },
    getCounter = function(row){
      level <- row[[private$chosen]]
      if (is.null(private$children)){
        private$counter
      } else {
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
    children = NA,
    counter = NA
  ))

#'
#'Implementation of DPRandomDecisionTreeClassifier. An S3 class.
#'
DPRandomDecisionTreeClassifier <- function(Y, X, epsilon = NULL, mechanism = NULL,
                                           numTrees = 5, height = ncol(X)/2){

  classLabels <- names(table((Y)))

  #grow trees
  trees <- c()
  i <- 1
  while(i <= numTrees) {
    tree <- RandomDecisionTree$new(predictors=names(X))
    tree$printTree()
    #train the tree
    for(row in 1:nrow(X)){
      tree$updateStats(row=X[row,], label=Y[row])
    }
    tree$printTree()
    trees<-c(trees, tree)
    i=i+1
  }
  self <- list(trees = trees, classLabels = classLabels)

  class(self) <- append("DPRandomDecisionTreeClassifier", class(self))
  return(self)
}

predict.DPRandomDecisionTreeClassifier <- function(object, testSetX = NULL, ...){

  trees <- object$trees

  predictOneData <- function(data){
    countsByClass <- lapply(trees, function(tree){ tree$getCounter(data)})
    countsByClass <- Reduce("+", countsByClass)
    probabilities <- countsByClass / sum(countsByClass)
    prediction <- object$classLabels[which.max(probabilities)]
  }

  apply(testSetX, 1, predictOneData)
}


test <- function(){

  #Example, remove later

  A <- c('1','0','1','0','1','1','1','0')
  B <- c('0','0','0','1','1','1','0','0')
  C <- c('1','1','0','0','1','1','1','1')
  D <- c('0','0','0','0','0','0','0','1')
  E <- c('0','0','0','1','0','1','1','0')
  Y <-c('T','F','T','F','T','F','T','T')

  X <- as.data.frame(cbind(A,B,C,D,E))

  rdtDP <- DPRandomDecisionTreeClassifier(Y, X)

  predictions <- predict(rdtDP, X)

}




