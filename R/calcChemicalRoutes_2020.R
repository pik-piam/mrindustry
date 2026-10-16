#'
#'Joins magpie objects with ammonia, methanol, HVC (Mt) and fertilizer production routes (MtN),
#'calculates ammonia and methanol final routes (Mt that are not used for HVC/fertilizer production),
#'converts from Mt to Gt and returns magpie object for year 2020.
#'
#' @author Qianzhi Zhang
#'
calcChemicalRoutes_2020<- function() {
  
  # ---------------------------------------------------------------------------
  # Load Route Data for Individual Chemicals
  #    - Retrieve ammonia, methanol, hvcs, and overall fertilizer data.
  # ---------------------------------------------------------------------------
  ammonia_route <- toolAggregateToEU21(calcOutput("AmmoniaRoute", aggregate = FALSE, supplementary = TRUE)) %>%
    as.data.frame() %>%
    select(-"Cell")
  
  methanol_route <- toolAggregateToEU21(calcOutput("MethanolRoute", aggregate = FALSE, supplementary = TRUE)) %>%
    as.data.frame() %>%
    select(-"Cell")
  
  hvc_route <- toolAggregateToEU21(calcOutput("HVCRoute", aggregate = FALSE, supplementary = TRUE)) %>%
    as.data.frame() %>%
    select(-"Cell")
  
  nfert_all <- toolAggregateToEU21(calcOutput("FertilizerRoute", aggregate = FALSE, supplementary = TRUE)) %>% 
    as.data.frame() %>%
    select(-"Cell")
  
  # ---------------------------------------------------------------------------
  # Split fertilizer Data into Production and Conversion Ratio
  # ---------------------------------------------------------------------------
  # fertilizer Production Data: rename 'NFert_production' to 'fertProd'
  nfert_route <- nfert_all %>% 
    filter(.data$Data1 == "NFert_production") %>%  
    mutate(Data1 = "fertProd")
  
  # fertilizer Conversion Ratio Data
  nfert_ratio <- nfert_all %>%
    filter(.data$Data1 == "NFert_ratio")
  
  
  # ---------------------------------------------------------------------------
  # Process Ammonia Route Data
  #    - Sum ammonia production by Region and Year.
  #    - Combine with fertilizer conversion ratio to compute the final ammonia flow
  # ---------------------------------------------------------------------------
  ammonia_total <- ammonia_route %>%
    group_by(.data$Region, .data$Year) %>%
    summarise(Total_Ammonia = sum(.data$Value, na.rm = TRUE), .groups = "drop")
  
  ammonia_tofinal <- ammonia_total %>%
    left_join(nfert_ratio, by = c("Region", "Year")) %>%  # Join by Region and Year
    mutate(amToFinal = .data$Total_Ammonia * (1 - .data$Value)) %>%     # Calculate final ammonia after conversion
    select(.data$Region, .data$Year, .data$amToFinal) %>%                     # Keep only relevant columns
    pivot_longer(cols = c("amToFinal"), 
                 names_to = "Data1", 
                 values_to = "Value")
  
  
  # ---------------------------------------------------------------------------
  # Process Methanol Route Data
  #    - Sum methanol production by Region and Year.
  #    - Calculate the final methanol flow by subtracting the methanol flow that is used to produce HVC via mtoMta
  #       mtoMta flow is in MtHVC and has to be converted to MtCH3OH
  # ---------------------------------------------------------------------------
  methanol_total <- methanol_route %>%
    group_by(.data$Region, .data$Year) %>%
    summarise(Total_Methanol = sum(.data$Value, na.rm = TRUE), .groups = "drop")
  
  hvc_MTO <- hvc_route %>%
    filter(.data$Data1 == "mtoMta")
  
  methanol_tofinal <- methanol_total %>%
    left_join(hvc_MTO, by = c("Region", "Year")) %>%   # Join by Region and Year
    mutate(meToFinal = .data$Total_Methanol - 2.62 * .data$Value) %>%  # Adjust methanol using a fixed conversion factor (2.62) #Dutta2019 Figure 2, Page 196
    select("Region", "Year", "meToFinal") %>%                  # Keep only relevant columns
    pivot_longer(cols = c("meToFinal"), 
                 names_to = "Data1", 
                 values_to = "Value")
  
  
  # ---------------------------------------------------------------------------
  # Combine All Chemical Routes
  #    - Combine original route data with the adjusted ammonia and methanol outputs.
  # ---------------------------------------------------------------------------
  Chemical_all <- rbind(ammonia_route, methanol_route, hvc_route, nfert_route, ammonia_tofinal, methanol_tofinal)
  
  # Add an additional column for processing: set 'opmoPrc' to "standard"
  Chemical_all <- Chemical_all %>%
    mutate(opmoPrc = "standard")%>%
    select("Region","Year","Data1","opmoPrc","Value")
  
  
  # ---------------------------------------------------------------------------
  # Load Total Chemical Production Data for Weighting
  # ---------------------------------------------------------------------------
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)[, "y2020", ]
  
  
  # ---------------------------------------------------------------------------
  # Convert Data to a Magpie Object and Disaggregate to Country Level
  #    - Convert the combined data to a magpie object.
  #    - Select the year 2020 and disaggregate the regional (EU21) data using chemical production weights.
  # ---------------------------------------------------------------------------
  x <- as.magpie(Chemical_all, spatial = 1, temporal = 2, datacol = 5)

  x <- x[, "y2020", ]

  x <- toolDisaggregateFromEU21(x, weight = Chemcial_Total)
  x[is.na(x)] <- 0  # Replace NA values with 0
  
  # Convert from Mt to Gt (megatonnes to gigatonnes)
  x <- x / 1000
  
  
  # ---------------------------------------------------------------------------
  # Return Final Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Routes & GtN (fertilizer)",
    description = "Aggregated chemical route data integrating ammonia, methanol, hvcs, and fertilizer routes. Data are adjusted by conversion factors and calculated at the resolution of regionmapping_21_EU11 and disaggregated to country level for 2020 (scaled from Mt to Gt)."
  ))
}
