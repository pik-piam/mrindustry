#' Calculate Country-Level Plastic Incineration Rate Trajectories
#'
#' Build time series of plastic incineration rates by sector and region
#' using OECD EoL data and external sources, then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
calcMPlIncinRate <- function() {
  # ---------------------------------------------------------------------------
  # Define sectors and regions
  #    - Retrieve manufacturing sectors (excluding 'Total') and regional codes.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping("structuremappingPlasticManu.csv", type = "sectoral", where = "mrindustry")
  targets <- setdiff(unique(sector_map$Target), "Total")

  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  regions <- unique(region_map$RegionCode)

  # ---------------------------------------------------------------------------
  # Load OECD incineration data and extend to 2020
  #    - Filter for 'Incinerated' fate and replicate 2019 to 2020.
  # ---------------------------------------------------------------------------
  incin_df <- calcOutput("MPlOECD_EoL", aggregate = TRUE) %>%
    as.data.frame() %>%
    dplyr::select(-Cell) %>%
    dplyr::filter(Data1 == "Incinerated") %>%
    dplyr::select(-Data1) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))

  incin_ext <- dplyr::bind_rows(
    incin_df,
    dplyr::filter(incin_df, Year == 2019) %>% dplyr::mutate(Year = 2020)
  )

  # ---------------------------------------------------------------------------
  # Compute historical share from external datasets (2005–2020)
  #    - Load EU, China, and US EoL CSVs, compute incineration share per region-year.
  # ---------------------------------------------------------------------------
  eu <- readSource("PlasticsEurope", subtype="PlasticEoL_EU", convert=FALSE) %>%
    as.data.frame() %>%
    dplyr::mutate(Region = "EUR", Year = as.integer(as.character(Year)))
  cn <- readSource("China_PlasticEoL", convert=FALSE) %>%
    as.data.frame() %>%
    dplyr::mutate(Region="CHA", Year=as.integer(as.character(Year)))
  us <- readSource("US_EPA", convert=FALSE) %>%
    as.data.frame()%>%
    dplyr::mutate(Region="USA", Year=as.integer(as.character(Year)))

  ext_all <- dplyr::bind_rows(eu, cn, us) %>%
    dplyr::filter(Year >= 2005, Year <= 2020) %>%
    dplyr::group_by(Region, Year) %>%
    dplyr::summarise(
      total = sum(Value, na.rm = TRUE),
      inc = sum(Value[Data1 == "Incinerated"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(share = inc / total) %>%
    dplyr::select(Region, Year, share)

  # ---------------------------------------------------------------------------
  # Merge ext shares into incin_ext, replacing where available
  # ---------------------------------------------------------------------------
  # TODO: interpolate non-OECD data, otherwise only specific years get replaced
  incin_hist <- incin_ext %>%
    dplyr::left_join(ext_all, by = c("Region", "Year")) %>%
    dplyr::mutate(Value = if_else(!is.na(share), share, Value)) %>%
    dplyr::select(Region, Year, Value)

  # ---------------------------------------------------------------------------
  # Fill 1990–2000 for non-CHA regions and extend to 2100
  #    - Copy Year 2000 value to 1990–1999; linearly interpolate from 2020 to 2100 to reach target 30%.
  # ---------------------------------------------------------------------------
  # Base 2000
  base2000 <- incin_hist %>%
    dplyr::filter(Year == 2000) %>%
    dplyr::select(Region, val2000 = Value)

  hist_ext <- incin_hist %>%
    dplyr::left_join(base2000, by = "Region") %>%
    dplyr::mutate(
      Value = if_else(Region != "CHA" & Year >= 1990 & Year < 2000, val2000, Value)
    ) %>%
    dplyr::select(-val2000)

  # Future 2021–2100
  target_share <- 0.30
  fut <- expand.grid(Region = regions, Year = 2021:2100, stringsAsFactors = FALSE) %>%
    dplyr::left_join(
      hist_ext %>% dplyr::filter(Year == 2020) %>% dplyr::select(Region, start = Value),
      by = "Region"
    ) %>%
    dplyr::mutate(
      Value = start + (Year - 2020) * (target_share - start) / (2100 - 2020)
    ) %>%
    dplyr::select(Region, Year, Value)

  final_df <- dplyr::bind_rows(
    hist_ext %>% dplyr::filter(Year < 2021),
    fut
  )

  # ---------------------------------------------------------------------------
  # Expand df by material
  # ---------------------------------------------------------------------------
  exp_df <- crossing(final_df, targets) %>%
    dplyr::select(Region, Year, targets, Value)

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to countries
  # ---------------------------------------------------------------------------
  x <- as.magpie(exp_df, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = region_map, dim = 1, from = "RegionCode", to = "CountryCode")

  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1

  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Plastic incineration",
    description = "Plastic incineration rate trajectories aggregated to country level for 1990–2100.",
    note        = "dimensions: (Time,Region,Material,value)"
  ))
}

