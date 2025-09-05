#' Extrapolates Chemical Flows from 2020 to 2005-2020 based on total chemical UE in 2005-2020
#' and assuming constant UE shares of the final chemical flows;
#' intermediate ammonia and methanol flows are calculated from the final flows and 
#' ammonia-to-fertilizer and methanol-to-hvc ratios.
#' Flows are aggregated to the country level.
#' 
#' @author Qianzhi Zhang
#'
calcAllChemicalFlows_2005to2020 <- function() {
  
  # ---------------------------------------------------------------------------
  # Define Conversion Factors (p37_mat2ue)
  #    - Conversion factors (mat2ue) for each product in 2005$/kg (or 2005$/kgN)
  # ---------------------------------------------------------------------------
  
  p37_mat2ue <- data.frame(
    Product = c("hvc", "fertilizer", "methFinal", "ammoFinal","OtherChem"),
    mat2ue = c(0.66, 0.73, 0.37, 0.69, 1),  # Conversion factors
    Unit = c("2017$/kg", "2017$/kgN", "2017$/kg", "2017$/kg","2017$/2017$")
  )
  
  # ---------------------------------------------------------------------------
  # Retrieve and Combine AllChemicalUeShare_2020 and Industry Demand Data
  #    - Get AllChemicalUeShare_2020 data (rounded to 8 digits) and remove extra columns.
  #    - Get industry demand (feIndustry) for selected years.
  #    - Join these datasets and calculate Material_Flow as:
  #         Material_Flow = (Value.x from AllChemicalUeShare_2020 * Value.y from feIndustry) / mat2ue
  # ---------------------------------------------------------------------------
  AllChemicalUeShares_2020 <- calcOutput("AllChemicalUeShares_2020", round = 8, aggregate = TRUE) %>% 
    as.data.frame() %>%
    select(-"Cell", -"Year")
  
  feIndustry <- calcOutput("FeDemandIndustry", scenarios="SSP2",signif = 4, warnNA = FALSE, aggregate = TRUE)[, c("y2005", "y2010", "y2015", "y2020"), "SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-"Cell")
  
  AllChemicalFlow <- AllChemicalUeShares_2020 %>%
    left_join(feIndustry, by = c("Region" = "Region")) %>%
    left_join(p37_mat2ue, by = c("Data1.x" = "Product")) %>%
    mutate(Material_Flow = .data$Value.x * .data$Value.y / .data$mat2ue)
  
  # ---------------------------------------------------------------------------
  # Calculate Ammonia Flow Based on fertilizer Conversion Ratio and ammoFinal Flow
  #    - Retrieve the NFert_ratio from FertilizerRoute (for year 2020).
  #    - Join the NFert_ratio to AllChemicalFlow (by Region).
  #    - For rows where Data1.x is "ammoFinal", adjust Material_Flow by dividing by (1 - NFert_ratio)
  #      and rename Data1.x to "ammonia".
  #    - Bind these adjusted rows back with the original dataset and remove temporary NFert_ratio.
  # ---------------------------------------------------------------------------
  nfert_ratio <- calcOutput("FertilizerRoute", aggregate = TRUE)[, "y2020", ] %>% 
    as.data.frame() %>%
    select(-"Cell", -"Year") %>%
    filter(.data$Data1 == "NFert_ratio")
  
  AllChemicalFlow <- AllChemicalFlow %>%
    left_join(
      nfert_ratio %>% 
        filter(.data$Data1 == "NFert_ratio") %>%
        select("Region", "NFert_ratio" = "Value"),
      by = "Region"
    ) %>%
    # Adjust rows for ammoFinal (ammonia)
    filter(.data$Data1.x == "ammoFinal") %>%
    mutate(
      Data1.x = "ammonia",
      Material_Flow = .data$Material_Flow / (1 - .data$NFert_ratio)
    ) %>%
    bind_rows(AllChemicalFlow) %>%   # Bind back the adjusted rows
    select(-"NFert_ratio")
  
  # ---------------------------------------------------------------------------
  # Calculate ratio of methanol (MeFinalratio) that goes to methFinal in order to then calculate methanol flow based on methFinal flow
  #    - Retrieve AllChemicalRoutes_2020 data for 2020 and remove extra columns.
  #    - From AllChemicalRoutes_2020, filter rows for "meToFinal" and "mtoMta".
  #    - Pivot these values wider to have separate columns for meToFinal and mtoMta.
  #    - Calculate MeFinalratio as:
  #         if meToFinal is 0 then 1(to avoid dividing by 0 later), else meToFinal / (mtoMta * 2.62 + meToFinal)
  # ---------------------------------------------------------------------------
  ChemicalRoutes_2020 <- calcOutput("ChemicalRoutes_2020", aggregate = TRUE)[, "y2020", ] %>% 
    as.data.frame() %>%
    select(-"Cell", -"Data2", -"Year")
  
  MeFinalratio <- ChemicalRoutes_2020 %>%
    filter(.data$Data1 %in% c("meToFinal", "mtoMta")) %>%
    pivot_wider(names_from = "Data1", values_from = "Value") %>%
    mutate(
      MeFinalratio = ifelse(.data$meToFinal == 0, 1, .data$meToFinal / (.data$mtoMta * 2.62 + .data$meToFinal))
    ) %>%
    select("Region", "MeFinalratio")
  
  # ---------------------------------------------------------------------------
  # Calculate methanol flow based on methFinal flow and MeFinalratio
  #    - Join the MeFinalratio with AllChemicalFlow (by Region).
  #    - For rows where Data1.x is "methFinal", adjust Material_Flow by dividing by MeFinalratio
  #      and rename Data1.x to "methanol".
  #    - Bind the new rows with AllChemicalFlow and remove the temporary MeFinalratio column.
  # ---------------------------------------------------------------------------
  AllChemicalFlow <- AllChemicalFlow %>%
    left_join(MeFinalratio, by = "Region")
  
  AllChemicalFlow<- AllChemicalFlow %>%
    bind_rows(
      AllChemicalFlow %>%
        filter(.data$Data1.x == "methFinal") %>%
        mutate(
          Data1.x = "methanol",
          Material_Flow = .data$Material_Flow / .data$MeFinalratio
        )
    ) %>%
    select(-"MeFinalratio")  # Remove temporary MeFinalratio column
  
  
  # ---------------------------------------------------------------------------
  # Finalize Output and Aggregate Data to Country Level
  #    - Select the required columns (Region, Year, Data1.x, Material_Flow).
  #    - Retrieve ChemicalTotal data for weighting.
  #    - Get the regional mapping and convert FinalOutput to a magpie object.
  #    - Aggregate the regional data to country level using the mapping and ChemicalTotal weights.
  # ---------------------------------------------------------------------------
  FinalOutput <- AllChemicalFlow %>%
    select("Region", "Year", "Data1.x", "Material_Flow")
  
  Chemical_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)[, c("y2005", "y2010", "y2015", "y2020"), ]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(FinalOutput, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", 
                     weight = Chemical_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0
  
  # ---------------------------------------------------------------------------
  # Return the Final Output
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Flows",
    description = "Chemical material flows from 2005 to 2020 extrapolated from 2020 based on total chemical UE and constant UE shares aggregated to the country level."
  ))
}

