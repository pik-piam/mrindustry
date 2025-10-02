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
  validSections <- c("drivers","steel","cement","plastic")

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

  #  ------------- PLASTIC -----------
  if (runSection("plastic")) {
    # Consumption
    calcOutput("MPlConsumptionByGood", file = "plastic_consumption.cs4r")
    # Trade
    calcOutput("MPlTrade",category = "final", flow_label = "Exports",file = "plastic_final_his_exports.cs4r")
    calcOutput("MPlTrade",category = "final", flow_label = "Imports",file = "plastic_final_his_imports.cs4r")
    calcOutput("MPlTrade",category = "primary", flow_label = "Exports",file = "plastic_primary_his_exports.cs4r")
    calcOutput("MPlTrade",category = "primary", flow_label = "Imports",file = "plastic_primary_his_imports.cs4r")
    calcOutput("MPlTrade",category = "intermediate", flow_label = "Exports",file = "plastic_intermediate_his_exports.cs4r")
    calcOutput("MPlTrade",category = "intermediate", flow_label = "Imports",file = "plastic_intermediate_his_imports.cs4r")
    calcOutput("MPlTrade",category = "manufactured", flow_label = "Exports",file = "plastic_manufactured_his_exports.cs4r")
    calcOutput("MPlTrade",category = "manufactured", flow_label = "Imports",file = "plastic_manufactured_his_imports.cs4r")
    calcOutput("MPlWasteTrade",subtype = "export",file = "plastic_waste_exports.cs4r")
    calcOutput("MPlWasteTrade",subtype = "import",file = "plastic_waste_imports.cs4r")
    # Parameters
    calcOutput("MPlOECD_MGshare",file = "plastic_material_shares_in_goods.cs4r")
    calcOutput("MPlMechReYield",round = 2, file = "plastic_mechanical_recycling_yield.cs4r") # fix 0.79
    calcOutput("MPlMechLoss",file = "plastic_reclmech_loss_uncontrolled_rate.cs4r")
    # Historic EoL shares
    calcOutput("MPlEoL_shares", subtype="Collected", file = "plastic_hist_collection_rate.cs4r")
    calcOutput("MPlEoL_shares", subtype="Recycled", file = "plastic_hist_mechanical_recycling_rate.cs4r")
    calcOutput("MPlEoL_shares", subtype="Incinerated", file = "plastic_hist_incineration_rate.cs4r")
    # EoL shares including extrapolations (to be moved to the MFA soon)
    calcOutput("MPlCollRate", file = "plastic_collection_rate.cs4r")
    calcOutput("MPlMechReRate", file = "plastic_mechanical_recycling_rate.cs4r")
    calcOutput("MPlIncinRate", file = "plastic_incineration_rate.cs4r")
    # Future rates (historic = 0)
    calcOutput("MPlChemReRate",file = "plastic_chemical_recycling_rate.cs4r")
    calcOutput("MPlBioRate",file = "plastic_bio_production_rate.cs4r")
    calcOutput("MPlDACRate",file = "plastic_daccu_production_rate.cs4r")
    # TODO
    # lifetime, carbon content materials, and emission capture rate
  }

}
