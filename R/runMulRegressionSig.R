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

  # Loop over signatures
  results <- data.frame()
  for (signature in colnames(signatures)[2:ncol(signatures)]) {
    # Create temporary data.frame
    f <- unlist(strsplit(formula, " "))
    condition <- f[2]
    if (length(f)==2) { # No covariates to asjust for
      dat <- merge(signatures[, c("ID", signature)],
                   p[, c("ID", condition)])
    } else {
      covariates <- f[seq(4, length(f), 2)]
      dat <- merge(signatures[, c("ID", signature)],
                   p[, c("ID", condition, covariates)])
    }

    model <- exposomeChallenge::runRegressionSig(dat, formula, family)
    summ <- summary(model)
    pval.condition <- summ$coefficients[2, 4]

    # Store p-value in data.frame
    results[signature, condition] <- pval.condition
  }

  return(results)
}
