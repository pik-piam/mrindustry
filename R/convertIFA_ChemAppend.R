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

    # Weights for all years of x: years after the last available weight year use the
    # last available year (constant extrapolation)
    weight <- time_interpolate(urea[unique(map$CountryCode), , ], getYears(x),
                               extrapolation_type = "constant")

    xNew <- toolAggregate(x, rel = map, from = "IFAReg", to = "CountryCode", weight = weight)
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
    getNames(ammoniaN) <- NULL  
    getNames(ureaN) <- NULL  
    weightData <- ammoniaN - ureaN 
    # in some countries (e.g. MMR, BOL, AZE) the reported urea N exceeds ammonia N, i.e. no ammonia is left for AN, CAN and AS
    weightData[is.na(weightData) | weightData < 0] <- 0

    # Weights for all years of x: years after the last available weight year use the
    # last available year (constant extrapolation)
    weight <- time_interpolate(weightData[unique(map$CountryCode), , ], getYears(x),
                               extrapolation_type = "constant")

    xNew <- toolAggregate(x, rel = map, from = "IFAReg", to = "CountryCode", weight = weight,
                          zeroWeight = "allow") # production of countries mapped to "Total Various" is 0, therefore the weight sum is zero; but so is the fertilizer production

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
