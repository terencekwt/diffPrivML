#' A categorical dataset containing breast cancer data.
#'
#' UCI machine learning repository dataset. Features are computed from a digitized
#' image of a fine needle aspirate (FNA) of a breast mass. They describe characteristics
#' of the cell nuclei present in the image.
#'
#' @name BreastCancer
#' @format Entries of patients
#' \describe{
#'   \item{Diagnosis}{benign vs malignant}
#'   \item{radius}{mean of distances from center to points on the perimeter}
#'   \item{texture}{standard deviation of gray-scale values}
#'   \item{perimeter}{perimeter of mass}
#'   \item{area}{area of mass}
#'   \item{smoothness}{local variation in radius lengths}
#'   \item{compactness}{perimeter^2 / area - 1.0}
#'   \item{concavity}{severity of concave portions of the contour}
#'   \item{concave points}{number of concave portions of the contour}
#'   \item{symmetry}{symmtery}
#'   \item{fractal dimension}{"coastline approximation" - 1}
#'   ...
#' }
NULL
