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

  ueChemicals <- calcOutput("FeDemandIndustry", scenarios=c("SSP2"), warnNA = FALSE, aggregate = FALSE)[,, "SSP2.ue_chemicals"]

  feIndustry <- ueChemicals %>%
    as.data.frame() %>%
    select(-"Cell") %>%
    mutate(Year = as.numeric(as.character(.data$Year))) %>%   # Convert factor to character then numeric
    group_by(.data$Region) %>%
    mutate(Ratio = .data$Value / .data$Value[.data$Year == 2020]) %>%
    ungroup()

  IEA_Petrochem_methanol <- calcOutput("IEA_Petrochem", subtype ="production5type_Methanol", aggregate = FALSE)[,,] %>%
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

  IEA_Petrochem_ammonia <- calcOutput("IEA_Petrochem", subtype ="production5type_Ammonia", aggregate = FALSE)[,,] %>%
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
    calcOutput("IEA_Petrochem", subtype = "production5type_Ethylene", aggregate = FALSE) +
      calcOutput("IEA_Petrochem", subtype = "production5type_Propylene", aggregate = FALSE) +
      calcOutput("IEA_Petrochem", subtype = "production5type_BTX", aggregate = FALSE)
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

  MagPie_Fert <- calcOutput("MAgPIEReport", subtype="fertilizer", aggregate = FALSE)[,,"SSP2.rcp45"]%>%
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
    # countries whose chemical UE demand vanishes (Ratio.fe = 0) would give Inf here, keep them constant instead
    dplyr::mutate(fe_change = ifelse(is.finite(.data$Ratio / .data$Ratio.fe), .data$Ratio / .data$Ratio.fe, 1)) %>%
    mutate(Data1 = case_when(
      .data$Data1 == "ammonia" ~ "ammoFinal",
      .data$Data1 == "methanol" ~ "methFinal",
      TRUE ~ .data$Data1
    ))%>%
    dplyr::left_join(p37_mat2ue, by = c("Data1" = "Product")) %>%
    # a country whose production of this chemical vanishes (fe_change = 0) would give Inf,
    # keep the baseline conversion factor for those instead
    dplyr::mutate(new_mat2ue = ifelse(.data$fe_change > 0, .data$mat2ue / .data$fe_change, .data$mat2ue))

  # Extend the data: For each Region and Data1 group, ensure rows exist for 2050, 2055, ..., 2150. (assume increase of ammonia and methanol final demand is the same as of ue_chemicals)
  years <- merged_data %>% select("Year") %>% distinct() %>% filter(.data$Year>2050)
  extended_years <- merged_data %>% filter(.data$Year==2050, .data$Data1 %in% c("ammoFinal","methFinal","hvc")) %>%
    select(-"Year") %>% crossing(years)
  final_data <- merged_data %>%
    rbind(extended_years) %>%
    filter(.data$Year >= 2020) %>%
    mutate(all_in = "ue_chemicals"
    )

  x <- as.magpie(final_data %>% select("Region","Year","Data1","all_in","new_mat2ue"),
                 spatial = 1, temporal = 2)

  # ---------------------------------------------------------------------------
  # Set Weighting and Return Final Output
  #    - mat2ue converts a material mass into UE value, so the regional factor is the mean
  #      over countries weighted by the projected production of that material:
  #      sum(mat2ue * production) / sum(production).
  # ---------------------------------------------------------------------------
  materialProduction <- final_data %>%
    mutate(Value = ifelse(is.finite(.data$Value) & .data$Value > 0, .data$Value, 0)) %>%
    select("Region","Year","Data1","all_in","Value")

  weight <- as.magpie(materialProduction, spatial = 1, temporal = 2)
  weight[is.na(weight)] <- 0
  weight <- weight[getItems(x, dim = 1), getItems(x, dim = 2), getItems(x, dim = 3)] + 1e-10

  return(list(
    x = x,
    weight = weight,
    unit = "2017$/kg or 2017$/kgN",  # Specify units based on conversion factors
    description = "Calculates the material-to-UE conversion factors for 2020-2150 on country level."
  ))
}

