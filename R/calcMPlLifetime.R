#' Calculates the mean lifetimes of plastic goods by use sector, or their standard deviations.
#' @author Leonie Schweiger
#' @param subtype Character string specifying to read means or standard deviations from data
#'        - "Lifetime_mean"
#'        - "Lifetime_std"
calcMPlLifetime <- function(subtype) {
  data <- readSource("Geyer", subtype=subtype, convert=FALSE)
  weight <- data
  weight[, , ] <- 1
  description <- paste(
    subtype,
    " of plastic goods by use sector. ",
    "Data from Geyer et al. 2017 https://doi.org/10.1126/sciadv.1700782"
  )
  output <- list(
    x = data,
    weight = weight,
    unit = "years (a)",
    description = description,
    isocountries = FALSE,
    note = "dimensions: (Good,value)"
  )
  return(output)
}
