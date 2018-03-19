#' @include noiseFunctions.R
NULL

#'Implementation of DPLogisticRegressionClassifier. An S3 class.
#'
#'@author Eva Gong
#'
#'@param y y variable with binary outcome
#'@param x x vairable
#'@param lambda regularization variable
#'@param alpha 0 for ridge and 1 for lasso
#'@param epsilon privacy budget in epsilon-differential private procedure
#'
#'@return a classifier object
#'@importFrom glmnet glmnet
#'@importFrom stats coef
#'@export
#'
#'@examples
#'data(iris)
#'x = as.matrix(iris[,1:4])
#'y = ifelse( iris$Species=="setosa", 1,0 )
#'lrClassifier = DPLogisticRegressionClassifier(y,x,lambda = 1, alpha = 0, epsilon = 1)
#'
DPLogisticRegressionClassifier <- function(y, x, lambda, alpha, epsilon = 0){
  param.out <- NULL
  n <- nrow(x)
  d <- ncol(x) + 1 # plus one since model matrix doesn't include intercept matrix
  model <- glmnet(x, y, family = ("binomial"),alpha=0, lambda = 1)
  param.out <- as.matrix(coef(model))
  if(epsilon>0){
    noise <- outputNoise(n,d,lambda,epsilon)
    param.out <- param.out + noise
  }

  self <- list(a0 = param.out[1,1],
               beta = param.out[2:d,1],
               lambda = lambda,
               classnames = model$classnames,
               x = x,
               y = y
               #call = model$call
               )
  class(self) <- append("DPLogisticRegressionClassifier", class(self))
  return(self)
}


#'summary of logistic regression
#'
#'@author Yifan Gong
#'
#'@param object the classifier object for this predict method
#'@param ... not applicable for this class
#'@export
#'
#'@examples
#'data(iris)
#'x = as.matrix(iris[,1:4])
#'y = ifelse( iris$Species=="setosa", 1,0 )
#'lrClassifier = DPLogisticRegressionClassifier(y,x,lambda = 1, alpha = 0, epsilon = 1)
#'summary(lrClassifier)
# rewrite summary versus the default summary method?
summary.DPLogisticRegressionClassifier <- function(object, ...){
  NextMethod()
  #stopifnot(inherits(x, "DPNaiveBayesClassifier"))
  cat("## a0:", object$a0, "\n")
  cat("## beta:", object$beta, "\n")
  cat("## classnames:", object$classnames, "\n")
  #cat("## call:", object$call, "\n")
}


#'predict of logistic regression
#'
#'@author Yifan Gong
#'
#'@param object the classifier object for this predict method
#'@param predictX a data frame in which to look for variables with which to predict. If omitted, data used for model fitting will be used
#'@param type the type of prediction. Can be either classes or probabilities
#'@param ... not applicable for this class
#'@export
#'
#'@examples
#'data(iris)
#'x = as.matrix(iris[,1:4])
#'y = ifelse( iris$Species=="setosa", 1,0 )
#'lrClassifier = DPLogisticRegressionClassifier(y,x,lambda = 1, alpha = 0, epsilon = 1)
#'summary(lrClassifier)
#'result = predict(lrClassifier,type = "classes")
#'table(result,y)
predict.DPLogisticRegressionClassifier <- function(object, predictX = NULL, type = c("classes","probabilities"), ...){
  coefficients <- as.matrix(c(object$a0,object$beta))
  print(coefficients)
  if (is.null(predictX)){
    x_in <- cbind(1,object$x)
  }else{x_in <- cbind(1,predictX)}
  if(dim(x_in)[2] != dim(coefficients)[1]) print("ERROR")
  probabilities <- as.vector(1/(1 + exp(-(x_in %*% coefficients))))
  classes <- (probabilities > 0.5) + 0
  if(type == "classes") return(classes)
  else return(probabilities)
}






