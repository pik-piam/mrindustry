#' Calculates mat2ue conversion factors of ammoFinal, methFinal, HVC and fertilizer
#' for 2020-2150 based on the mat2ue conversion factors in 2020 and the 
#' projected relative increases in production (IEA The Future of Petrochemicals) and 
#' total chemical UE (FeDemandIndustry)
#' 
#' @author Qianzhi Zhang
#'
calcAllChemicalMat2Ue_2020to2150 <- function() {

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
  
  # ---------------------------------------------------------------------------
  # Retrieve Chemical production projections for 2020-2050 (extrapolate between 2017 and 2025 if 2020 is missing)
  # - total Chemical UE projection from calcFeDemandIndustry
  # - Methanol, HVC & Ammonia projections from IEA_Petrochem
  # - Fertilizer demand projections from MagPie
  # and calculate change compared to baseline year
  # ---------------------------------------------------------------------------
  
  feIndustry <- calcOutput("FeDemandIndustry", scenarios=c("SSP2"), warnNA = FALSE, aggregate = TRUE)[,, "SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-"Cell") %>%
    mutate(Year = as.numeric(as.character(.data$Year))) %>%   # Convert factor to character then numeric
    group_by(.data$Region) %>%
    mutate(Ratio = .data$Value / .data$Value[.data$Year == 2020]) %>%
    ungroup()
  
  IEA_Petrochem_methanol <- calcOutput("IEA_Petrochem", subtype ="production5type_Methanol", aggregate = TRUE)[,,] %>%
    as.data.frame() %>%
    select(-"Cell", -"Data1") %>%
    filter(!.data$Year %in% c("X2015")) %>%
    mutate(Year = as.numeric(gsub("X", "", .data$Year))) %>%
    group_by(.data$Region) %>%
    # Ensure rows for 2017, 2020, and 2025 exist (if 2020 is missing, it will be created)
    tidyr::complete(Year = c(2017, 2020, 2025)) %>%
    arrange(.data$Region, .data$Year) %>%
    # For each region, interpolate the missing 2020 value if necessary
    mutate(Value = ifelse(is.na(.data$Value) & .data$Year == 2020,
                          .data$Value[.data$Year == 2017] + (.data$Value[.data$Year == 2025] - .data$Value[.data$Year == 2017]) * (2020 - 2017) / (2025 - 2017),
                          .data$Value)) %>%
    ungroup() %>%
    group_by(.data$Region) %>%
    # Use the 2020 value as the baseline (i.e. Ratio = 1 for 2020)
    mutate(Ratio = .data$Value / .data$Value[.data$Year == 2020]) %>%
    ungroup()%>%
    mutate(Data1 = "methanol") %>%
    filter(!.data$Year %in% 2017)
  
  IEA_Petrochem_ammonia <- calcOutput("IEA_Petrochem", subtype ="production5type_Ammonia", aggregate = TRUE)[,,] %>%
    as.data.frame() %>%
    select(-"Cell", -"Data1") %>%
    filter(!.data$Year %in% c("X2015")) %>%
    mutate(Year = as.numeric(gsub("X", "", .data$Year))) %>%
    group_by(.data$Region) %>%
    # Ensure rows for 2017, 2020, and 2025 exist (if 2020 is missing, it will be created)
    tidyr::complete(Year = c(2017, 2020, 2025)) %>%
    arrange(.data$Region, .data$Year) %>%
    # For each region, interpolate the missing 2020 value if necessary
    mutate(Value = ifelse(is.na(.data$Value) & .data$Year == 2020,
                          .data$Value[.data$Year == 2017] + (.data$Value[.data$Year == 2025] - .data$Value[.data$Year == 2017]) * (2020 - 2017) / (2025 - 2017),
                          .data$Value)) %>%
    ungroup() %>%
    group_by(.data$Region) %>%
    # Use the 2020 value as the baseline (i.e. Ratio = 1 for 2020)
    mutate(Ratio = .data$Value / .data$Value[.data$Year == 2020]) %>%
    ungroup()%>%
    mutate(Data1 = "ammonia") %>%
    filter(!.data$Year %in% 2017)
  
  IEA_Petrochem_hvc <- (
    calcOutput("IEA_Petrochem", subtype = "production5type_Ethylene", aggregate = TRUE) +
      calcOutput("IEA_Petrochem", subtype = "production5type_Propylene", aggregate = TRUE) +
      calcOutput("IEA_Petrochem", subtype = "production5type_BTX", aggregate = TRUE)
  ) %>%
    as.data.frame() %>%
    select(-"Cell", -"Data1") %>%
    filter(!.data$Year %in% c("X2015")) %>%
    mutate(Year = as.numeric(gsub("X", "", .data$Year))) %>%
    group_by(.data$Region) %>%
    # Ensure rows for 2017, 2020, and 2025 exist (if 2020 is missing, it will be created)
    tidyr::complete(Year = c(2017, 2020, 2025)) %>%
    arrange(.data$Region, .data$Year) %>%
    # For each region, interpolate the missing 2020 value if necessary
    mutate(Value = ifelse(is.na(.data$Value) & .data$Year == 2020,
                          .data$Value[.data$Year == 2017] + (.data$Value[.data$Year == 2025] - .data$Value[.data$Year == 2017]) * (2020 - 2017) / (2025 - 2017),
                          .data$Value)) %>%
    ungroup() %>%
    group_by(.data$Region) %>%
    # Use the 2020 value as the baseline (i.e. Ratio = 1 for 2020)
    mutate(Ratio = .data$Value / .data$Value[.data$Year == 2020]) %>%
    ungroup()%>%
    mutate(Data1 = "hvc") %>%
    filter(!.data$Year %in% 2017)
  
  MagPie_Fert <- calcOutput("MAgPIEReport", subtype="fertilizer")[,,"SSP2.rcp45"]%>%
    as.data.frame()%>%
    select(-"Cell", -"Data1", -"Data2")%>%
    mutate(Year = as.numeric(as.character(.data$Year))) %>%
    group_by(.data$Region) %>%
    mutate(Ratio = .data$Value / .data$Value[.data$Year == 2020]) %>%
    ungroup() %>%
    mutate(Data1 = "fertilizer")
  
  # ---------------------------------------------------------------------------
  # Compute future mat2ue by dividing the baseline by the relative change in UE chemicals demand of the respective chemical
  # ---------------------------------------------------------------------------
  merged_data <- rbind(IEA_Petrochem_methanol, IEA_Petrochem_ammonia, IEA_Petrochem_hvc, MagPie_Fert) %>%
    dplyr::left_join(feIndustry, by = c("Region", "Year"), suffix = c("", ".fe")) %>%
    dplyr::mutate(fe_change = ifelse(is.nan(.data$Ratio / .data$Ratio.fe), 1, .data$Ratio / .data$Ratio.fe)) %>%
    mutate(Data1 = case_when(
      .data$Data1 == "ammonia" ~ "ammoFinal",
      .data$Data1 == "methanol" ~ "methFinal",
      TRUE ~ .data$Data1
    ))%>%
    dplyr::left_join(p37_mat2ue, by = c("Data1" = "Product")) %>%
    dplyr::mutate(new_mat2ue = .data$mat2ue / .data$fe_change)
  
  # Extend the data: For each Region and Data1 group, ensure rows exist for 2050, 2055, ..., 2150. (assume increase of ammonia and methanol final demand is the same as of ue_chemicals)
  years <- merged_data %>% select("Year") %>% distinct() %>% filter(.data$Year>2050)
  extended_years <- merged_data %>% filter(.data$Year==2050, .data$Data1 %in% c("ammoFinal","methFinal","hvc")) %>%
    select(-"Year") %>% crossing(years)
  final_data <- merged_data %>% 
    rbind(extended_years) %>%
    filter(.data$Year >= 2020) %>%
    mutate(all_in = "ue_chemicals"
    ) %>%
    select("Region","Year","Data1","all_in","new_mat2ue")
  
  x <- as.magpie(final_data, spatial = 1, temporal = 2)
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
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
    unit = "2017$/kg or 2017$/kgN",  # Specify units based on conversion factors
    description = "Calculates the material-to-UE conversion factors for 2020-2150 on country level."
  ))
}

