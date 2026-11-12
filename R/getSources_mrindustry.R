#' Get Sources mrindustry
#'
#' Lists the sources that each calc function of mrindustry reads, directly or through functions of other
#' madrat packages. The list is stored in `inst/mrindustry_sources.csv` and commited. To update it, run
#' `getSources_mrindustry("inst/mrindustry_sources.csv")` in the package folder.
#'
#' @param file Path of the csv file the list is written to. If `NULL`, the list is only returned.
#' @return A data frame with one row per calc function: its name in `CalcFunction` and the comma-separated
#' names of its sources in `Source`.
#' @author Tobias Diez
#' @md
#' @export
getSources_mrindustry <- function(file = NULL) {
  # radix sorting is independent of the locale, which keeps the stored list stable across machines
  calcs <- sort(grep("^calc", ls(getNamespace("mrindustry")), value = TRUE), method = "radix")
  # the dependency analysis warns about mappings it cannot find, which do not affect the sources
  sources <- suppressWarnings(lapply(calcs, function(calc) sort(getSources(calc)$source, method = "radix")))
  table <- data.frame(
    CalcFunction = calcs,
    Source = vapply(sources, paste, character(1), collapse = ", ")
  )
  if (!is.null(file)) {
    utils::write.csv(table, file, row.names = FALSE)
  }
  return(table)
}
