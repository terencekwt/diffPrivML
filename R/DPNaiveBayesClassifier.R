#'Implementation of DPNaiveBayesClassifier
#'
#'@param data a data frame
#'@param mechanism function for DF
#'@param budget privacy budget
#'
#'@return a classifier
#'
#'@import caret
#'
#'@export
#'
#'@examples
#'
#'
DPNaiveBayesClassifier <- function(data, mechanism, budget){
  ## to be implemented and perhaps convert to s3 class
}

## example of simple naive bayes from scratch on iris dataset,
## will need to modify this to be DP and generalize it,
## but this is a good start
library(caret)

#preprocessing
data(iris)
dataset <- iris
training_indices <-createDataPartition(dataset$Species, p = 0.70, list = FALSE)
test_set <- dataset[-training_indices, ]
training_set <- dataset[training_indices, ]

#training
training_x <- training_set[,1:4]
training_y <- training_set[,5]
class_labels <- names(table(training_y))

#P(C_j)
prior_prob <- as.vector(table(training_y)) / nrow(training_set)

#mean and sd for all P(a_i|C_j)
likelihood <-
  sapply(names(training_x), function(feature_name) {
    feature <- training_set[[feature_name]]
    if (is.numeric(feature)) {
      tab <- rbind(tapply(feature, training_y, mean),
                   tapply(feature, training_y, sd))
      rownames(tab) <- c("mean", "sd")
      names(dimnames(tab)) <- c(feature_name, "")
      as.table(tab)
    }
    else {
      ## it is nominal attributes
    }
  }, simplify = FALSE)

#prediction

test_x <- training_set[,1:4]
test_y <- training_set[,5]

predict<- function(data){
  pnorms <- sapply(class_labels, function(y) {
    prod(sapply(names(test_x), function(x){
      dnorm(data[[x]], mean = likelihood[[x]]['mean',y], sd = likelihood[[x]]['sd',y])
    }))
  })
  posterior_prob <- prior_prob * pnorms
  prediction <- class_labels[which.max(posterior_prob)]
  prediction
}
predictions <- apply(test_x, 1, predict)
accuracy <- sum(predictions==test_y)/length(test_y)
