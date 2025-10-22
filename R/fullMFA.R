#' Load all MFA data
#' @description
#' Function that produces the complete regional data sets required for the 
#' MFA model.
#' @param run_sections Character vector selecting which parts to run.
#'   Allowed: c("drivers","steel","cement"). NULL (default) runs all.
#'
#' @author Merlin HOSAK
#' @author Bennet Weiss
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
fullMFA <- function(rev = 0, dev = "", scenario='SSP2', gdp_per_capita=FALSE, run_sections=NULL) {

  # prepare section selector
  validSections <- c("drivers","steel","cement")

  if (is.null(run_sections)) {
    sections <- validSections
  } else {
    bad <- setdiff(run_sections, validSections)
    if (length(bad)) stop("Invalid sections: ", paste(bad, collapse=", "))
  }

  runSection <- function(name) name %in% run_sections

  if (!length(run_sections)) {
    message("fullMFA: no sections selected; nothing done.")
    return(invisible(NULL))
  }

  #  ------------- DRIVERS -------------
  if (runSection("drivers")) {
    calcOutput("Population1900To2150", file = "co_population1900To2150.cs4r", scenario=scenario)
    calcOutput("GDP1900To2150", file = "co_gdp1900To2150.cs4r", scenario=scenario, per_capita=gdp_per_capita)
  }

  #  ------------- STEEL ----------------
  if (runSection("steel")) {
    # Production
    calcOutput("SteelProduction", file = "st_steel_production.cs4r")
    # Trade
    calcOutput("SteelTrade", file = "st_steel_imports.cs4r", subtype='imports')
    calcOutput("SteelTrade", file = "st_steel_exports.cs4r", subtype='exports')
    calcOutput("SteelTrade", file = "st_steel_scrap_imports.cs4r", subtype='scrap_imports')
    calcOutput("SteelTrade", file = "st_steel_scrap_exports.cs4r", subtype='scrap_exports')
    calcOutput("SteelTrade", file = "st_steel_indirect_imports.cs4r", subtype='indirect_imports')
    calcOutput("SteelTrade", file = "st_steel_indirect_exports.cs4r", subtype='indirect_exports')
    # Parameters
    calcOutput("SteelStaticParameters", file = "st_steel_static_parameters.cs4r")
    calcOutput("CullenFabricationYield", file = "st_fabrication_yield.cs4r", aggregate=FALSE)
    calcOutput("SteelLifetimes", subtype='Cooper2014', file = "st_lifetimes.cs4r", aggregate=FALSE)
    calcOutput("SteelRecoveryRate", subtype='WorldSteel', file = "st_recovery_rate.cs4r", aggregate=FALSE)
    calcOutput("SteelSectorSplits", subtype='Pauliuk2013', file = "st_sector_splits.cs4r", aggregate=FALSE)
  }

  #  ------------- CEMENT -----------
  if (runSection("cement")) {
    # Production
    calcOutput("MCeBinderProduction", file = "ce_cement_production.cs4r", years=1900:2023, subtype="cement")
    # Trade
    calcOutput("MCeMaterialTrade", file = "ce_cement_trade.cs4r", years=1900:2023, subtype="cement")
    calcOutput("MCeMaterialTrade", file = "ce_clinker_trade.cs4r", years=1900:2023, subtype="clinker")
    # Parameters
    calcOutput("MCeBuiltLifespan", file = "ce_use_lifetime_mean.cs4r")
    calcOutput("MCeClinkerRatio", file = "ce_clinker_ratio.cs4r", years=1900:2023)
  }

}