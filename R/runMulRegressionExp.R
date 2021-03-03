#' Function to run multiple GLMs
#'
#' Function to run multiple GLMs of the form signature ~ exposure.
#'
#' @import Biobase
#' @import rexposome
#' @import dplyr
#' @export

runMulRegressionExp <- function(signatures, exposome, covariates, family) {
  # Extract signatures
  signatures <- data.frame(t(Biobase::exprs(signatures)))
  signatures <- tibble::rownames_to_column(signatures, "ID")

  # Extract exposures
  exposures <- data.frame(rexposome::expos(exposome))[, 1:10]
  exposures <- tibble::rownames_to_column(exposures, "ID")

  # Extract phenotype
  p <- data.frame(Biobase::pData(exposome))
  p <- tibble::rownames_to_column(p, "ID")

  # Loop over signatures
  results <- data.frame()
  for (signature in colnames(signatures)[2:ncol(signatures)]) {
    # Loop over exposures
    for (exposure in colnames(exposures)[2:ncol(exposures)]) {

      # Create temporary data.frame and formula for GLM
      if (covariates == "") {
        formula <- paste(signature, "~", exposure)
        dat <- merge(signatures[, c("ID", signature)],
                     exposures[, c("ID", exposure)])
      } else {
        formula <- paste(signature, "~", exposure, "+", covariates)

        new.covariates <- unlist(strsplit(covariates, " "))
        dat <- merge(signatures[, c("ID", signature)],
                     exposures[, c("ID", exposure)],
                     p[, c("ID", new.covariates)])
      }

      model <- exposomeChallenge::runRegressionSig(dat, formula, family)
      summ <- summary(model)
      pval.condition <- summ$coefficients[2, 4]

      # Store p-values in data.frame
      results[signature, exposure] <- pval.condition

      # Adjust for multiple testing
    }
  }

  return(results)
}
