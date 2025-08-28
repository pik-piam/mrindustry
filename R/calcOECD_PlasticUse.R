#' Calculate Country-Level Plastic Use by Sector
#'
#' Backcast total plastic use from 1990-2019 back to 1950.
#' Combine sectoral use shares and total use to compute absolute plastic use
#' by sector at country level for 1950-2019.
#'
#' @author Qianzhi Zhang
#'
calcOECD_PlasticUse <- function() {
  # ---------------------------------------------------------------------------
  # Load and clean sectoral share data
  #    - Read share output and prepare dataframe, rename Data2 to Data1 to align dimensions.
  # ---------------------------------------------------------------------------
  share_df <- calcOutput(
    "OECD_PlasticUseShare", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::select(-Cell,-Year)
  
  # ---------------------------------------------------------------------------
  # Load total use data 1990-2019
  # Load global production data 1950-2015 (Geyer et al. 2017 as reference)
  # Backcast total use data to 1950
  # ---------------------------------------------------------------------------
  total <- calcOutput("OECD_PlasticUseTotal", aggregate = TRUE) 
  Geyer <- readSource("Geyer", subtype="Prod_1950-2015") 
  total_df <- toolBackcastByReference2D(total, Geyer) %>%
    as.data.frame() %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))%>%
    dplyr::select(-Cell,-Data1)

  # ---------------------------------------------------------------------------
  # Combine shares and totals to compute sectoral use
  #    - Join on Region, Year, and Data1, calculate Value = Share * Total.
  # ---------------------------------------------------------------------------
  combined <- share_df %>%
    dplyr::rename(Share = Value) %>%
    dplyr::right_join(
      total_df %>% dplyr::rename(Total = Value),
      by = c("Region"),
      relationship = "many-to-many"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Value = Share * Total) %>%
    dplyr::select(Region, Year, Data1, Value)
  
  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to country level
  #    - Map regions to countries using GDP weights.
  # ---------------------------------------------------------------------------
  x <- as.magpie(combined, spatial = 1, temporal = 2)[, paste0("y", 1960:2019),] # use only data back to 1960 because no GDP weights available before
  
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  gdp_weights <- calcOutput(
    "GDP", scenario="SSP2", average2020 = FALSE, aggregate = FALSE
  )[, paste0("y", 1960:2019) , "SSP2"]
  
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode",
    weight = gdp_weights[unique(region_map$CountryCode), , ]
  )
  
  # ---------------------------------------------------------------------------
  # Return output and metadata
  # ---------------------------------------------------------------------------
  return(list(
    x           = x,
    weight      = NULL,
    unit        = "Mt Plastic",
    description = "Sectoral plastic use aggregated to country level for 1990-2019."
  ))
}
