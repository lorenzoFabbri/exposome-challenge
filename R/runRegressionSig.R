#' Function to run single GLM
#'
#' Function to run single GLM of the form signature ~ condition.
#'
#' @export

runRegressionSig <- function(dat, formula, family) {
  model <- glm(formula, data = dat, family = family)

  return(model)
}
