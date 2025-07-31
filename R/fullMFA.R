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
fullMFA <- function(scenario='SSP2', gdp_per_capita=FALSE) {
  
  #  ------------- DRIVERS -------------
  calcOutput("Population1900To2150", file = "population1900To2150.cs3r", 
             scenario=scenario)
  calcOutput("GDP1900To2150", file = "gdp1900To2150.cs3r", 
             scenario=scenario, 
             per_capita=gdp_per_capita)
  
  #  ------------- STEEL ----------------
  
    # Production
  
  calcOutput("SteelProduction", file = "steel_production.cs3r")
  
    # Trade
  
  calcOutput("SteelTrade", file = "steel_imports", subtype='imports')
  calcOutput("SteelTrade", file = "steel_exports", subtype='exports')
  calcOutput("SteelTrade", file = "steel_scrap_imports", subtype='scrap_imports')
  calcOutput("SteelTrade", file = "steel_scrap_exports", subtype='scrap_exports')
  calcOutput("SteelTrade", file = "steel_indirect_imports", subtype='indirect_imports')
  calcOutput("SteelTrade", file = "steel_indirect_exports", subtype='indirect_exports')
  
  
  #  ------------- PARAMETERS -----------
  
}