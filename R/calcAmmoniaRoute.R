#'
#' Calculates ammonia production volumes per production route in Mt for 2015-2020
#' based on IFA total ammonia production volumes (2015-2020) and
#' shares for different ammonia production routes from IEA (2020 and 2017) and China specific data (2020)
#'
#' @author Qianzhi Zhang
#'
calcAmmoniaRoute <- function() {
  # ============================================================================
  # Get IFA ammonia production volumes in Kt, convert to Mt and select the years 2015-2020.
  # ============================================================================
  ammoniaProduction <- (calcOutput("IFA_Chem", subtype = "ammonia_statistics_production",
                                   aggregate = FALSE) / 1000)[, c("y2015", "y2016", "y2017",
                                                                  "y2018", "y2019", "y2020"), ]

  # countries that actually produce ammonia and therefore need a route mix
  producers <- getItems(ammoniaProduction, dim = 1)[as.vector(dimSums(ammoniaProduction, dim = c(2, 3))) > 0]

  ammonia_production <- ammoniaProduction %>%
    as.data.frame()

  # ============================================================================
  # Get IEA shares for different ammonia production routes for the Base Year 2020 (IEA_Ammonia)
  #   - IEA_Ammonia reports no Asia Pacific region, so a number of producing countries carry no
  #     data at all (currently BGD, PAK, IDN, JPN, MYS, MMR, VNM, AUS, NZL) and are taken from
  #     IEA_Petrochem below. Which countries those are is derived from the actual data coverage
  #     rather than from region codes, so this stays independent of the regionmapping set in the
  #     config and cannot silently drop a producer that happens to sit in an unlisted region.
  # ============================================================================
  ieaAmmonia <- calcOutput("IEA_Ammonia", subtype = "BaseYear_2020", aggregate = FALSE)[, "y2020", ]
  ieaAmmoniaCountries <- getItems(ieaAmmonia, dim = 1)[as.vector(dimSums(ieaAmmonia, dim = 3)) > 0]

  ammonia_share_iea <- ieaAmmonia %>%
    as.data.frame() %>%
    # for China there is more specific data (RMI_China)
    filter(.data$Region %in% setdiff(ieaAmmoniaCountries, "CHN"))

  # Map Data1 values to REMIND categories
  ammonia_share_iea$Category <- case_when(
    ammonia_share_iea$Data1 %in% c("Coal", "CCU") ~ "amSyCoal",
    ammonia_share_iea$Data1 %in% c("Coal_with_CCS") ~ "amSyCoal_cc",
    ammonia_share_iea$Data1 %in% c("Gas", "Gas_with_CCU", "Pyrolysis") ~ "amSyNG", # Methane pyrolysis: H2 is produced for ammonia production and black carbon for other potential uses
    ammonia_share_iea$Data1 %in% c("Gas_with_CCS") ~ "amSyNG_cc",
    ammonia_share_iea$Data1 %in% c("Oil") ~ "amSyLiq",
    ammonia_share_iea$Data1 %in% c("Electrolysis") ~ "amSyH2"
  )

  # ----------------------------------------------------------------------------
  # Group by Region, Year, and Category, then calculate normalized share (%)
  # ----------------------------------------------------------------------------
  ammonia_share_iea <- ammonia_share_iea %>%
    group_by(.data$Region, .data$Year, .data$Category) %>%
    summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(.data$Region, .data$Year) %>%
    mutate(normalized_value = (.data$Value / sum(.data$Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-"Value", -"Year")

  # ============================================================================
  # Fall back to IEA shares from IEA_Petrochem (2017) for producers without IEA_Ammonia data
  #   - the output of IEA_Petrochem (subtype="RouteRTS_Ammonia") is Mt of ammonia output per production route
  #   - all ammonia production from coal comes from China, but IEA_Petrochem reports it for the
  #     whole region China sits in, so the disaggregation falsely hands it to the other countries
  #     of that region. amSyCoal is therefore set to 0 for those (China itself takes RMI_China
  #     data below, so it is not affected).
  # ============================================================================
  petroMap <- toolGetMapping("regionmappingIEA_Petrochem_5region.csv", type = "regional", where = "mrindustry")
  coalLeakage <- petroMap$CountryCode[petroMap$IFAReg %in% petroMap$IFAReg[petroMap$CountryCode == "CHN"]]

  ammonia_share_ieapetro <- calcOutput("IEA_Petrochem", subtype = "RouteRTS_Ammonia", aggregate = FALSE)[, "y2017", ] %>%
    as.data.frame() %>%
    filter(.data$Region %in% setdiff(producers, c(ieaAmmoniaCountries, "CHN")))

  # Map Data1 values to REMIND categories for petrochemical routes
  ammonia_share_ieapetro$Category <- case_when(
    ammonia_share_ieapetro$Data1 %in% c("Coal_GS", "CCU") ~ "amSyCoal",
    ammonia_share_ieapetro$Data1 %in% c("NG_SR", "COG_SR", "Bio_GS", "Pyrolysis") ~ "amSyNG",
    ammonia_share_ieapetro$Data1 %in% c("Oil_SR") ~ "amSyLiq"
  )

  # Group by Region, Year, and Category:
  #    - Sum the values, then drop the coal route where it only reflects Chinese production.
  #    - calculate the share per production route for each region-year group.
  ammonia_share_ieapetro <- ammonia_share_ieapetro %>%
    group_by(.data$Region, .data$Year, .data$Category) %>%
    summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>% # Sum values by group
    mutate(Value = ifelse(.data$Category == "amSyCoal" & .data$Region %in% coalLeakage,
                          0, .data$Value)) %>%
    group_by(.data$Region, .data$Year) %>%
    mutate(normalized_value = (.data$Value / sum(.data$Value, na.rm = TRUE)) * 100) %>% # Normalize
    ungroup() %>%
    select(-"Value", -"Year")

  # ============================================================================
  # Get China-specific ammonia data (Base Year 2020)
  # ============================================================================
  ammonia_share_china <- calcOutput("RMI_China", subtype = "ChemStructure_Ammonia", aggregate = FALSE)["CHN", "y2020", ] %>%
    as.data.frame()

  # Map Data1 values to REMIND categories for China data
  ammonia_share_china$Category <- case_when(
    ammonia_share_china$Data1 %in% c("Coal") ~ "amSyCoal",
    ammonia_share_china$Data1 %in% c("Coal + CCS") ~ "amSyCoal_cc",
    ammonia_share_china$Data1 %in% c("Gas", "Others") ~ "amSyNG",
    ammonia_share_china$Data1 %in% c("Gas + CCS") ~ "amSyNG_cc",
    ammonia_share_china$Data1 %in% c("PtX") ~ "amSyH2"
  )

  # Group by Region, Year, and Category, then normalize the share values
  ammonia_share_china <- ammonia_share_china %>%
    group_by(.data$Region, .data$Year, .data$Category) %>%
    summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(.data$Region, .data$Year) %>%
    mutate(normalized_value = (.data$Value / sum(.data$Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-"Value", -"Year")

  # ============================================================================
  # Combine all ammonia share data (IEA, IEA Petrochem, and China)
  # ============================================================================
  ammonia_share_all <- rbind(ammonia_share_iea, ammonia_share_ieapetro, ammonia_share_china)

  # ============================================================================
  # Guard: a producer without a route mix loses its production silently
  # ============================================================================
  uncovered <- setdiff(producers, unique(ammonia_share_all$Region[ammonia_share_all$normalized_value > 0]))
  if (length(uncovered) > 0) {
    warning("No ammonia production route shares available for ", paste(uncovered, collapse = ", "),
            " (", signif(sum(ammoniaProduction[uncovered, , ]), 3), " Mt over ",
            paste(range(getYears(ammoniaProduction, as.integer = TRUE)), collapse = "-"),
            "). Their ammonia production is dropped from the route data.")
  }

  # ============================================================================
  # Calculate actual ammonia route values
  #   - Join the combined share data with production data.
  #   - Compute the actual value using the normalized share.
  # ============================================================================
  ammonia_route_value <- ammonia_share_all %>%
    left_join(ammonia_production, by = "Region", relationship = "many-to-many") %>%
    mutate(actual_value = .data$normalized_value * .data$Value / 100) %>%
    select(-"Value", -"Cell", -"Data1", -"normalized_value")

  # ============================================================================
  # Convert ammonia route data to a magpie object for spatial and temporal analysis
  #   - only countries with route shares are in the data frame, so the remaining ISO countries
  #     (which do not produce ammonia) are filled with 0
  # ============================================================================
  x <- as.magpie(ammonia_route_value, spatial = 1, temporal = 3)
  x[is.na(x)] <- 0 # Replace any NA values with 0
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  # ============================================================================
  # Return the final object along with metadata
  # ============================================================================
  return(list(
    x = x,
    weight = NULL,
    unit = "MtNH3",
    description = "Ammonia production distribution by production route, derived from IEA, petrochemical, and China-specific data."
  ))
}
