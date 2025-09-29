#' Calculates UE shares for all chemicals (including OtherChem) in 2020 from 
#' final product flows (methFinal, ammoFinal, HVC, fertilizer), 
#' material prices (mat2ue) and the total UE of chemicals (from FeDemandIndustry).
#' Shares are aggregated to the country level.
#' 
#' @author Qianzhi Zhang
#'
calcAllChemicalUeShares_2020 <- function() {
  
  # ---------------------------------------------------------------------------
  # Define Material-to-UE Conversion Factors
  #    - p37_mat2ue: Conversion factors (mat2ue) for selected products.
  #      The conversion factors are expressed in 2017$/kg or 2017$/kgN.
  # ---------------------------------------------------------------------------
  
  p37_mat2ue <- data.frame(
    Product = c("hvc", "fertilizer", "methFinal", "ammoFinal"),
    mat2ue = c(0.66, 0.73, 0.37, 0.69),  # Conversion factors
    Unit = c("2017$/kg", "2017$/kgN", "2017$/kg", "2017$/kg")
  )
  #ue_chemicals is measured in value_added (trn$2017), whilst material is measured in Gt
  #So this is the price of material in trn$2017/Gt = $2017/kg
  #compiled by Qianzhi from several sources, documentation in "Market price of chemicals.xlsx"
  #p37_mat2ue(t,all_regi,"hvc","ue_chemicals") = 0.66; !!2017$/kg Source: https://businessanalytiq.com/procurementanalytics/index/ethylene-price-index/
  #p37_mat2ue("fertilizer","ue_chemicals") = 0.73; !!2017$/kgN Source: https://farmdocdaily.illinois.edu/wp-content/uploads/2023/06/06132023_fig1,png 2020 Global Average
  #p37_mat2ue("methFinal","ue_chemicals") = 0.37; !!2017$/kg Source: https://www.methanex.com/about-methanol/pricing/ 2020 Global Average
  #p37_mat2ue("ammoFinal","ue_chemicals") = 0.69; !!2017$/kg Source: https://businessanalytiq.com/procurementanalytics/index/ammonia-price-index/ 2020 Global Average
  
  # ---------------------------------------------------------------------------
  # Calculate Chemical Flow UE for 2020
  #    - Retrieve ChemicalFlow data for 2020, remove unwanted columns, and filter out 
  #      rows for 'ammonia' and 'methanol' (since these are not processed here).
  #    - Join with the conversion factors and calculate the material-to-UE value.
  # ---------------------------------------------------------------------------
  ChemicalFlows_2020 <- calcOutput("ChemicalFlows_2020", aggregate = TRUE)[, "y2020", ] %>%
    as.data.frame() %>%
    select(-"Cell") %>%
    filter(!.data$Data1 %in% c("ammonia", "methanol")) %>%
    left_join(p37_mat2ue, by = c("Data1" = "Product")) %>%
    mutate(ue_material = .data$Value * .data$mat2ue)
  
  
  # ---------------------------------------------------------------------------
  # Retrieve Industrial Demand Data for Chemicals
  #    - Get the feIndustry data from the FeDemandIndustry output for 2020.
  # ---------------------------------------------------------------------------
  feIndustry <- calcOutput("FeDemandIndustry", warnNA = FALSE, aggregate = TRUE, scenarios=c("SSP2"))[, "y2020", "SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-"Cell")
  
  
  # ---------------------------------------------------------------------------
  # Calculate UE Share for Each Chemical Product
  #    - Join the chemical flow data with industry demand data (by Region).
  #    - Compute the share (ue_share) by dividing the ue_material by the corresponding industry demand.
  # ---------------------------------------------------------------------------
  AllChemicalUEShares_2020 <- ChemicalFlows_2020 %>%
    left_join(feIndustry, by = "Region") %>%
    mutate(ue_share = .data$ue_material / .data$Value.y) %>%   # 'Value.y' from feIndustry
    select("Region", "Year.x", "Data1.x", "ue_share")
  
  
  # ---------------------------------------------------------------------------
  # Account for Residual ("OtherChem") Share
  #    - For each region and year, sum the ue_share of existing products.
  #    - Create a new row for "OtherChem" representing the remaining share (1 minus the sum).
  # ---------------------------------------------------------------------------
  ue_summary <- AllChemicalUEShares_2020 %>%
    group_by(.data$Region, .data$Year.x) %>%
    summarise(
      ue_sum = sum(.data$ue_share, na.rm = TRUE),
      .groups = "drop"
    )
  
  AllChemicalUEShares_2020 <- AllChemicalUEShares_2020 %>%
    bind_rows(
      ue_summary %>%
        mutate(
          Data1.x = "OtherChem",       # New product name for residual share
          ue_share = 1 - .data$ue_sum         # Remaining share
        )
    ) %>%
    mutate(all_in = "ue_chemicals") %>%
    select("Region", "Year.x", "Data1.x", "all_in", "ue_share")
  
  
  # ---------------------------------------------------------------------------
  # Convert to Magpie Object and Aggregate to Country Level
  #    - Retrieve regional mapping.
  #    - Convert the UE data to a magpie object and aggregate from regions to countries.
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  x <- as.magpie(AllChemicalUEShares_2020, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode")
  
  # ---------------------------------------------------------------------------
  # Set Weighting and Return Final Output
  #    - Create a weight object with the same dimensions as 'x' (all values set to 1).
  #    - Return the aggregated magpie object along with metadata.
  # ---------------------------------------------------------------------------
  weight <- x  # Copy dimensions from x
  weight[, , ] <- 1
  
  return(list(
    x = x,
    weight = weight,
    unit = "share",  
    description = "Unit energy (UE) share of chemicals including OtherChem in 2020."
  ))
}

