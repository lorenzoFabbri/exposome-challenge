#' Function to run single GLM
#'
#' Function to run single GLM of the form signature ~ condition.
#'
#' @export

runRegressionSig <- function(dat, formula, family) {
  # Add dependent variable to formula
  formula <- paste0(colnames(dat)[2], formula)
  model <- glm(formula, data = dat, family = family)

  return(model)
}
