#' Calculates chemical route flows (including OtherChem) from 2005 to 2020 
#' from total chemical flows based on 2020 route shares.
#' Flows are aggregated to the country level.
#' 
#' @author Qianzhi Zhang
#'
#' @param CCS boolean parameter whether CCS technologies are considered as such in 2020 or assumed to be technologies without CCS
#' 
calcAllChemicalRoutes_2005to2020 <- function(CCS=FALSE) {
  
  # ---------------------------------------------------------------------------
  # Load and Prepare Chemical Flow Data (2005-2020)
  #    - Retrieve AllChemicalFlows_2005to2020 data, remove unnecessary columns,
  #      and rename "Data1" to "Product" for consistency.
  # ---------------------------------------------------------------------------
  AllChemicalFlows_2005to2020 <- calcOutput("AllChemicalFlows_2005to2020", warnNA = FALSE, aggregate = TRUE) %>% 
    as.data.frame() %>%
    select(-"Cell") %>%
    rename(Product = .data$Data1)
  
  # ---------------------------------------------------------------------------
  # Load and Recategorize Chemical Route Data for 2020
  #    - Retrieve ChemicalRoutes_2020 data for the year 2020.
  #    - Remove extra columns and recategorize "Data1" into broader product groups,
  #      creating a new "Product" column.
  # ---------------------------------------------------------------------------
  ChemicalRoutes_2020 <- calcOutput("ChemicalRoutes_2020", aggregate = TRUE)[, "y2020", ] %>% 
    as.data.frame() %>%
    select(-"Cell", -"Data2", -"Year") %>%
    mutate(
      Product = case_when(
        .data$Data1 %in% c("amSyCoal", "amSyNG", "amSyLiq", "amSyCoal_cc", "amSyNG_cc", "amSyH2") ~ "ammonia",
        .data$Data1 %in% c("meSySol", "meSyNg", "meSyLiq", "meSyH2", "meSySol_cc", "meSyNg_cc") ~ "methanol",
        .data$Data1 %in% c("mtoMta", "stCrNg", "stCrLiq") ~ "hvc",
        .data$Data1 == "amToFinal" ~ "ammoFinal",
        .data$Data1 == "meToFinal" ~ "methFinal",
        .data$Data1 == "fertProd" ~ "fertilizer",
        TRUE ~ .data$Data1  # Retain other values as is
      )
    )
  
  # ---------------------------------------------------------------------------
  # Calculate Share Within Each Product Group
  #    - Group the recategorized route data by Region, Product, and original Data1.
  #    - Sum the values and calculate the share of each route within its Product.
  # ---------------------------------------------------------------------------
  ChemicalRoutes_2020 <- ChemicalRoutes_2020 %>%
    group_by(.data$Region, .data$Product, .data$Data1) %>%
    summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(.data$Region, .data$Product) %>%
    mutate(
      Share = .data$Value / sum(.data$Value)
    )
  
  # ---------------------------------------------------------------------------
  # Join Flow Data with Route Shares and Compute Routes_Flow
  #    - Join the 2005-2020 flow data with the route shares by Region and Product.
  #    - Compute Routes_Flow as the product of the original flow (Value.x) and the share.
  #    - Set processing flag (opmoPrc) based on route type.
  # ---------------------------------------------------------------------------
  AllChemicalRoutes_2005to2020 <- AllChemicalFlows_2005to2020 %>%
    left_join(ChemicalRoutes_2020, by = c("Region", "Product")) %>%
    mutate(
      Routes_Flow = .data$Value.x * .data$Share,
      opmoPrc = "standard"
    ) %>%
    select("Region", "Year", "Data1", "opmoPrc", "Routes_Flow") %>%
    filter(!is.na(.data$Data1))
  
  # ---------------------------------------------------------------------------
  # Retrieve "OtherChem" Data
  # ---------------------------------------------------------------------------
  
  OtherChem <- AllChemicalFlows_2005to2020 %>% filter(.data$Product=="OtherChem") %>%
    rename(Data1="Product", Routes_Flow="Value")%>%
    mutate(opmoPrc = "standard", Data1="chemOld")
  
  # ---------------------------------------------------------------------------
  # Combine Route Data with "OtherChem"
  # ---------------------------------------------------------------------------
  AllChemicalRoutes_2005to2020 <- bind_rows(AllChemicalRoutes_2005to2020, OtherChem)
  
  # ---------------------------------------------------------------------------
  # If no CCS technologies are considered, assign the CCS technology to the respective non-CCS technology
  # ---------------------------------------------------------------------------
  if(CCS==FALSE){
    AllChemicalRoutes_2005to2020 <- AllChemicalRoutes_2005to2020 %>%
      group_by(.data$Region, .data$Year) %>%
      mutate(
        Routes_Flow = ifelse(.data$Data1 == "amSyNG",
                             .data$Routes_Flow + sum(.data$Routes_Flow[.data$Data1 == "amSyNG_cc"], na.rm = TRUE), 
                             .data$Routes_Flow),
        Routes_Flow = ifelse(.data$Data1 == "amSyNG_cc", 0, .data$Routes_Flow)
      ) %>%
      ungroup() 
  }
  
  # ---------------------------------------------------------------------------
  # Aggregate Data to Country Level
  #    - Retrieve ChemicalTotal data for weighting.
  #    - Get regional mapping and convert the combined data to a magpie object.
  #    - Aggregate from regions to country level using the mapping and weights.
  # ---------------------------------------------------------------------------
  Chemical_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)[, c("y2005", "y2010", "y2015", "y2020"), ]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(AllChemicalRoutes_2005to2020, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", 
                     weight = Chemical_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0
  
  # ---------------------------------------------------------------------------
  # Return the Final Aggregated Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Routes",
    description = "Chemical route flows from 2005 to 2020 including 'OtherChem'."
  ))
}

