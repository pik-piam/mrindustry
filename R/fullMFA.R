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
    calcOutput("Population1900To2150", file = "population1900To2150.cs4r", scenario=scenario)
    calcOutput("GDP1900To2150", file = "gdp1900To2150.cs4r", scenario=scenario, per_capita=gdp_per_capita)
  }

  #  ------------- STEEL ----------------
  if (runSection("steel")) {
    # Production
    calcOutput("SteelProduction", file = "steel_production.cs3r")
    # Trade
    calcOutput("SteelTrade", file = "steel_imports.cs3r", subtype='imports')
    calcOutput("SteelTrade", file = "steel_exports.cs3r", subtype='exports')
    calcOutput("SteelTrade", file = "steel_scrap_imports.cs3r", subtype='scrap_imports')
    calcOutput("SteelTrade", file = "steel_scrap_exports.cs3r", subtype='scrap_exports')
    calcOutput("SteelTrade", file = "steel_indirect_imports.cs3r", subtype='indirect_imports')
    calcOutput("SteelTrade", file = "steel_indirect_exports.cs3r", subtype='indirect_exports')
    # Parameters
    calcOutput("SteelStaticParameters", file = "steel_static_parameters.cs3r")
    calcOutput("CullenFabricationYield", file = "fabrication_yield.cs3r", aggregate=FALSE)
    calcOutput("SteelLifetimes", subtype='Cooper2014', file = "lifetimes.cs3r", aggregate=FALSE)
    calcOutput("SteelRecoveryRate", subtype='WorldSteel', file = "recovery_rate.cs3r", aggregate=FALSE)
    calcOutput("SteelSectorSplits", subtype='Pauliuk2013', file = "sector_splits.cs3r", aggregate=FALSE)
  }

  #  ------------- CEMENT -----------
  if (runSection("cement")) {
    # Production
    calcOutput("MCeBinderProduction", file = "cement_production.cs4r", years=1900:2023, subtype="cement")
    # Trade
    calcOutput("MCeMaterialTrade", file = "cement_trade.cs4r", years=1900:2023, subtype="cement")
    calcOutput("MCeMaterialTrade", file = "clinker_trade.cs4r", years=1900:2023, subtype="clinker")
    # Parameters
    calcOutput("MCeBuiltLifespan", file = "use_lifetime_mean.cs4r")
    calcOutput("MCeClinkerRatio", file = "clinker_ratio.cs4r", years=1900:2023)
  }

}