#' Convert IFA_Chem
#'
#' Convert IFA_Chem data to ISO country level.
#'
#' @param x MagPIE object containing IFA data at regional resolution.
#' @return MagPIE object of the IFA data disaggregated to country level.
#' @author Qianzhi Zhang
#' @inheritParams readIFA_ChemAppend
#'
convertIFA_ChemAppend <- function(x, subtype = x) {

  subtype <- unlist(strsplit(subtype, "_"))

  # ---------------------------------------------------------------------------
  # Process Based on Subtype: Capacities vs. Statistics
  # ---------------------------------------------------------------------------
  if (subtype[2] == "capacities") {
    # ------------------------------
    # Capacities Subtype Processing
    # ------------------------------
    # Read the regional mapping file for capacities
    map <- toolGetMapping("regionmappingIFA_Chem_othersC.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")

    # Retrieve weighting data from urea capacities
    urea <- calcOutput("IFA_Chem", subtype = "urea_capacities_capacities", aggregate = FALSE)

    # Determine available years from both datasets
    x_years <- as.numeric(sub("y", "", colnames(x)[grepl("^y\\d{4}$", colnames(x))]))
    urea_years <- as.numeric(sub("y", "", colnames(urea)[grepl("^y\\d{4}$", colnames(urea))]))
    start_year <- min(x_years, na.rm = TRUE)
    end_year   <- max(x_years, na.rm = TRUE)
    last_available_year <- max(urea_years, na.rm = TRUE)

    # Loop over each year and perform aggregation using appropriate weights
    for (year in start_year:end_year) {
      year_col <- paste0("y", year)
      weight_col <- if (year <= last_available_year) paste0("y", year) else paste0("y", last_available_year)

      if (weight_col %in% colnames(urea)) {
        aggregated_result <- toolAggregate(
          x[, year_col, drop = FALSE],
          rel = map,
          from = "IFAReg",
          to = "CountryCode",
          weight = urea[, weight_col, drop = FALSE][unique(map$CountryCode), , drop = FALSE]
        )
        if (year == start_year) {
          xNew <- aggregated_result
        } else {
          xNew <- mbind(xNew, aggregated_result)
        }
      } else {
        warning(paste("Weight column", weight_col, "is missing in urea data. Skipping aggregation for year", year, "."))
      }
    }
  } else if (subtype[2] == "statistics") {
    # ------------------------------
    # Statistics Subtype Processing
    # ------------------------------
    # Read the regional mapping file for statistics
    map <- toolGetMapping("regionmappingIFA_Chem_othersP.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")

    # use the ammonia N that is not used for urea production as weight:
    # AN, CAN and AS are produced from this remaining ammonia.
    ammoniaN <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", unitNitrogen = TRUE,
                            aggregate = FALSE)
    ureaN <- calcOutput("IFA_Chem", subtype = "urea_statistics_production", unitNitrogen = TRUE,
                        aggregate = FALSE)
    ammoniaN <- toolCountryFill(ammoniaN, fill = 0, verbosity = 2)
    ureaN <- toolCountryFill(ureaN, fill = 0, verbosity = 2)
    years <- intersect(getYears(ammoniaN), getYears(ureaN))
    weightData <- ammoniaN[, years, ] - setNames(ureaN[getItems(ammoniaN, dim = 1), years, ], getNames(ammoniaN))
    weightData[is.na(weightData) | weightData < 0] <- 0

    # Determine available years from both datasets
    x_years <- as.numeric(sub("y", "", colnames(x)[grepl("^y\\d{4}$", colnames(x))]))
    weight_years <- as.numeric(sub("y", "", colnames(weightData)[grepl("^y\\d{4}$", colnames(weightData))]))
    start_year <- min(x_years, na.rm = TRUE)
    end_year   <- max(x_years, na.rm = TRUE)
    last_available_year <- max(weight_years, na.rm = TRUE)

    # Loop over each year and perform aggregation using appropriate weights
    for (year in start_year:end_year) {
      year_col <- paste0("y", year)
      weight_col <- if (year <= last_available_year) paste0("y", year) else paste0("y", last_available_year)

      if (weight_col %in% colnames(weightData)) {
        aggregated_result <- toolAggregate(
          x[, year_col, drop = FALSE],
          rel = map,
          from = "IFAReg",
          to = "CountryCode",
          weight = weightData[, weight_col, drop = FALSE][unique(map$CountryCode), , drop = FALSE],
          zeroWeight = "allow" # production of countries mapped to "Total Various" is 0, therefore the weight sum is zero; but so is the fertilizer production
        )
        if (year == start_year) {
          xNew <- aggregated_result
        } else {
          xNew <- mbind(xNew, aggregated_result)
        }
      } else {
        warning(paste("Weight column", weight_col, "is missing in weighting data. Skipping aggregation for year", year, "."))
      }
    }

  } else {
    stop("Invalid subtype combination")
  }

  # ---------------------------------------------------------------------------
  # Finalize Output: Fill Missing Country Data with 0
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(xNew, fill = 0)

  # Return the processed MagPIE object
  return(x)
}
