#' @title convertStegmann2022
#' @description Converts data from Stegmann2022
#' @param x unconverted magpie object from read-script
#' @param subtype Character. "FEweighted" disaggregates all variables based on
#' FE; "PopWeighted" disaggregates all variables based on Population.
#'
#' @return magpie object with a completed dataset.
#'

convertStegmann2022 <- function(x, subtype) {
  x <- x[c("World"), , , invert = TRUE]

  regmapping <- toolGetMapping("regionmapping_IMAGE_PBL_Stegmann2022.csv", where = "mrremind", type = "regional")

  if (subtype == "PopWeighted") {
    weight <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[unique(regmapping$CountryCode), getYears(x), ]
  } else if (subtype == "FEweighted") {
    weight <- calcOutput("FE", aggregate = FALSE)[unique(regmapping$CountryCode), 2016, "FE (EJ/yr)"]
  } else {
    stop("Unknown subtype '", subtype, "'. Use 'PopWeighted' or 'FEweighted'.")
  }
  out <- toolAggregate(x, regmapping, from = "RegionAbbreviation", to = "CountryCode", weight = weight)
  out <- toolCountryFill(out, fill = 0, verbosity = 2)

  return(out)
}
