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
