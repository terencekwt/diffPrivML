#'
#'Laplace mechanism
#'
#'@param originalData data that needs to sensitized
#'@param sensitivity sensitivity
#'@param epsilon epsilon
#'
#'@importFrom stats rexp
#'
#'@return the perturbed data
#'
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

#'
#'Output pertubation mechanism
#'
#'@param n number of rows in the input data
#'@param d number of columns in the input data plus constant column
#'@param lambda regularization parameter
#'@param epsilon privacy budget in epsilon-differential private procedure
#'
#'@importFrom stats rnorm
#'
#'@return output noise to be added to the coefficients estimates
#'
outputNoise <- function(n,d,lambda,epsilon){
  dir = rnorm(d)
  dir = dir/ sqrt(sum(dir^2))
  noise = 2/(epsilon*lambda*n) * sum(rexp(d, 1)) * dir
  return(noise)
}
