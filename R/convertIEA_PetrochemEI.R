#' Convert IEA_PetrochemEI
#'
#' Convert IEA Chemical and Petrochemical Sector 2009 “Table 12. Petro Regional Coef” data as magclass object
#'
#' @param x MAgPIE object containing IEA_PetrochemEI region resolution
#' @return MAgPIE object of the IEA_PetrochemEI data disaggregated to country level
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertIEA_PetrochemEI(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertIEA_PetrochemEI <- function(x) {
  # ---------------------------------------------------------------------------
  # IEA Petrochem EI data is only given for specific countries (e.g. Brazil, Saudi Arabia),
  # which are assumed to be representative for their H12 Region. Data is therefore first mapped
  # to H12 regions and then further disaggregated to the ISO country level
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingIEA_PetrochemEI.csv", type = "regional", where = "mrindustry")
  x <- toolAggregate(x, rel = map, dim = 1, from = "IEACountry", to = "RegionCode")
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode")

  # ---------------------------------------------------------------------------
  # Return the Final MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
