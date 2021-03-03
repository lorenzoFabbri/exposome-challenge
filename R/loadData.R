#' Function to load data and create ExposomeSet object
#'
#' @import rexposome
#' @export

loadExpData <- function() {
  # Load Exposome dataset
  load("./inst/extdata/exposome.RData")

  # Load serum Metabolome dataset
  load("./inst/extdata/metabol_serum.Rdata")

  # Load urine Metabolome dataset
  load("./inst/extdata/metabol_urine.Rdata")

  # Create ExposomeSet
  exposome <- exposome[, -1]
  codebook <- codebook[, -1]
  rownames(phenotype) <- phenotype[, 1]
  phenotype <- phenotype[, -1]

  keep <- intersect(colnames(exposome), rownames(codebook))
  exposome <- exposome[, keep]
  codebook <- codebook[keep, ]

  exp <- rexposome::loadExposome(exposures = exposome,
                                 description = codebook,
                                 phenotype = phenotype,
                                 description.famCol = "family")

  return(list(exp = exp,
              serum = metabol_serum,
              urine = metabol_urine))
}
