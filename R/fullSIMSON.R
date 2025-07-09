#' Load all SIMSON data
#' @description
#' Function that produces the complete regional data sets required for the 
#' SIMSON model.
#'
#' @author Merlin HOSAK
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}}, 
#' \code{\link[madrat]{calcOutput}}, \code{\link[mrindustry]{calcSteelProduction}}
#' @export
#' @examples
#' \dontrun{
#' retrieveData('SIMSON')
#' fullSIMSON()
#' }
#'
fullSIMSON <- function(scenario='SSP2', gdp_per_capita=FALSE) {
  
  #  ------------- DRIVERS -------------
  calcOutput("Population200", file = "population200.cs3r", 
             scenario=scenario)
  calcOutput("GDP200", file = "gdp200.cs3r", 
             scenario=scenario, 
             per_capita=gdp_per_capita)
  
  #  ------------- STEEL ----------------
  
  calcOutput("SteelProduction", file = "steel_production.cs3r")
  
  #  ------------- PARAMETERS -----------
  
}