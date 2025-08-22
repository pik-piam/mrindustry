#' Load all MFA data
#' @description
#' Function that produces the complete regional data sets required for the 
#' MFA model.
#'
#' @author Merlin HOSAK
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}}, 
#' \code{\link[madrat]{calcOutput}}, \code{\link[mrindustry]{calcSteelProduction}}
#' @export
#' @examples
#' \dontrun{
#' retrieveData('MFA')
#' fullMFA()
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
  
  calcOutput("SteelTrade", file = "steel_imports.cs3r", subtype='imports')
  calcOutput("SteelTrade", file = "steel_exports.cs3r", subtype='exports')
  calcOutput("SteelTrade", file = "steel_scrap_imports.cs3r", subtype='scrap_imports')
  calcOutput("SteelTrade", file = "steel_scrap_exports.cs3r", subtype='scrap_exports')
  calcOutput("SteelTrade", file = "steel_indirect_imports.cs3r", subtype='indirect_imports')
  calcOutput("SteelTrade", file = "steel_indirect_exports.cs3r", subtype='indirect_exports')
  
  
  #  ------------- PARAMETERS -----------
  
}