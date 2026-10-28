#' Aggregates chemical flow data in 2020 from calcAllChemicalRoutes_2020into broader categories
#' based on their outputs (ammonia, methanol, HVC, fertilizer and final ammonia/methanol).
#' 
#' @author Qianzhi Zhang
#'
calcChemicalFlows_2020 <- function() {
  
  # ---------------------------------------------------------------------------
  # Load All Chemical Route Data
  #    - Retrieve the aggregated ChemicalRoutes_2020 data.
  #    - Remove the unwanted columns 'Cell' and 'Data2'.
  # ---------------------------------------------------------------------------
  ChemicalRoutes_2020 <- calcOutput("ChemicalRoutes_2020", regionmapping = "regionmapping_21_EU11.csv") %>%
    as.data.frame() %>%
    select(-"Cell", -"Data2")
  
  # ---------------------------------------------------------------------------
  # Re-categorize and Summarize Data
  #    - Reclassify Data1 into broader categories:
  #       * 'ammonia' for ammonia-related routes,
  #       * 'methanol' for methanol-related routes,
  #       * 'hvc' for hvc routes,
  #       * 'ammoFinal' for the final ammonia output,
  #       * 'methFinal' for the final methanol output,
  #       * 'fertilizer' for fertilizer production.
  #    - Group the data by Region, Year, and the new Data1 category,
  #      then sum the Value for each group.
  # ---------------------------------------------------------------------------
  ChemicalRoute_summarized <- ChemicalRoutes_2020 %>%
    mutate(Data1 = case_when(
      .data$Data1 %in% c("amSyCoal", "amSyNG", "amSyLiq", "amSyCoal_cc", "amSyNG_cc", "amSyH2") ~ "ammonia",
      .data$Data1 %in% c("meSySol", "meSyNg", "meSyLiq", "meSyH2", "meSySol_cc", "meSyNg_cc") ~ "methanol",
      .data$Data1 %in% c("mtoMta", "stCrNg", "stCrLiq") ~ "hvc",
      .data$Data1 == "amToFinal" ~ "ammoFinal",
      .data$Data1 == "meToFinal" ~ "methFinal",
      .data$Data1 == "fertProd" ~ "fertilizer",
      TRUE ~ .data$Data1
    )) %>%
    group_by(.data$Region, .data$Year, .data$Data1) %>%
    summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = "drop")
  
  # ---------------------------------------------------------------------------
  # Load Total Chemical Production Data for Weighting
  #    - Retrieve the ChemicalTotal data for 2020 to be used as weights.
  # ---------------------------------------------------------------------------
  Chemical_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)[, "y2020", ]
  
  # ---------------------------------------------------------------------------
  # Convert Data to Magpie Object and Disaggregate to Country Level
  #    - Convert the summarized data to a magpie object.
  #    - Disaggregate the regional (EU21) data to the country level using the weights.
  # ---------------------------------------------------------------------------
  x <- as.magpie(ChemicalRoute_summarized, spatial = 1, temporal = 2)
  x <- toolDisaggregateFromEU21(x, weight = Chemical_Total)
  x[is.na(x)] <- 0  # Replace any missing values with 0
  
  # ---------------------------------------------------------------------------
  # Return the Final Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Flows & GtN (fertilizer)",
    description = "Aggregated chemical flow data recategorized into key sectors (ammonia, methanol, hvc, final outputs, and fertilizer) calculated at the resolution of regionmapping_21_EU11 and disaggregated to country level for 2020."
  ))
}

