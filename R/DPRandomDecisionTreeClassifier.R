
#Example

A <- c(1,0,1,0,1,1,1,0)
B <- c(0,0,0,1,1,1,0,0)
C <- c(1,1,0,0,1,1,1,1)
D <- c(0,0,0,0,0,0,0,1)
E <- c(0,0,0,1,0,1,1,0)
class <-c('T','F','T','F','T','F','T','T')

X <- as.data.frame(cbind(A,B,C,D,E))

RandomDecisionTree <- R6Class("RandomDecisionTree",
  public = list(
    initialize = function(predictors) {
      private$chosen <- predictors[sample(length(predictors),1)]
      if(length(predictors) > 3) {
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
      level <- as.factor(row[[private$chosen]])
      if (is.null(private$children)){
        private$counter[label] <- private$counter[label] + 1
      } else {
        child <- private$children[[level]]
        child$updateStats(row=row, label=label)
      }
    },
    printTree = function(){
      if(is.null(private$children)){
        private$chosen
      } else {
        paste(private$chosen, "(", private$children[['0']]$printTree(), private$children[['1']]$printTree(), ")")
      }
    },
    getCounter = function(row){
      level <- as.factor(row[[private$chosen]])
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

#calculate the cardinality of each predictors
predictors <- table()

#initialize tree
set.seed(15)
tree <- RandomDecisionTree$new(predictors=names(X))
tree$printTree()
#train the tree
for(row in 1:nrow(X)){
  tree$updateStats(row=X[row,], label=class[row])
}
tree$getCounter(X[1,])



