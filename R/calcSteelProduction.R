#' Calc steel production from WorldSteelDigitised data
#' @author Merlin Jo Hosak
#' @param x Magpie object
calcSteelProduction <- function() {
  # TODO historic world production data needs to be split across countries
  # prod_hist <- readSource('WorldSteelDigitised', 'world_production')
  prod_70s = readSource('WorldSteelDigitised', 'production_1969-1979')
  prod_80s = readSource('WorldSteelDigitised', 'production_1980-1989')
  prod_90s = readSource('WorldSteelDigitised', 'production_1990-1999')
  prod_00s = readSource('WorldSteelDigitised', 'production_2000-2009')
  
  prod_recent <- mbind(prod_70s, prod_80s, prod_90s, prod_00s)
  
  return(prod_recent)
}