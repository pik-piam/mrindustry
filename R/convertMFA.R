#' Convert remind-mfa output to ISO country level
#'
#' Steel
#' and cement are already exported for all ISO countries and pass unchanged. Plastics
#' is exported on eu21 regions by the MFA, so its values are split to the countries of each region:
#' - `plastics_hvc_production` by HVC production in 2017 from `IEA_Petrochem`
#' - `plastics_waste` by GDP in 2020 (SSP2)
#'
#' remind-mfa does not run the SSP2 variants `SSP2_lowEn`, `SSP2_highDemDEU`,
#' `SSP2IndiaHigh` and `SSP2IndiaMedium` yet, so they get the SSP2 data until
#' the export contains them.
#'
#' @md
#' @param x magpie object returned by [readMFA()]
#' @inheritParams readMFA
#' @return magpie object on ISO country level, with the dimensions of `x`
#'   otherwise unchanged
#' @author Tobias Diez
#' @seealso [readMFA()], [toolDisaggregateFromEU21()]
convertMFA <- function(x, subtype) {
  weight <- switch(
    subtype,
    plastics_hvc_production = calcOutput("IEA_Petrochem", subtype = "production3type_All",
                                         aggregate = FALSE)[, "y2017", "HVCs"],
    plastics_waste = calcOutput("GDP", scenario = "SSP2", years = 2020, aggregate = FALSE)
  )
  if (!is.null(weight)) {
    x <- toolDisaggregateFromEU21(x, weight = weight)
  }

  aliases <- c("SSP2_lowEn", "SSP2_highDemDEU", "SSP2IndiaHigh", "SSP2IndiaMedium")
  for (scenario in setdiff(aliases, getItems(x, dim = "scenario"))) {
    alias <- x[, , list(scenario = "SSP2")]
    getItems(alias, dim = "scenario") <- scenario
    x <- mbind(x, alias)
  }
  return(x)
}
