#' Function to run multiple GLMs
#'
#' Function to run multiple GLMs of the form signature ~ condition.
#'
#' @import Biobase
#' @import dplyr
#' @export

runMulRegressionSig <- function(signatures, exposome, formula, family) {
  # Extract signatures
  signatures <- data.frame(t(Biobase::exprs(signatures)))
  signatures <- tibble::rownames_to_column(signatures, "ID")

  # Extract phenotype
  p <- data.frame(Biobase::pData(exposome))
  p <- tibble::rownames_to_column(p, "ID")
}
