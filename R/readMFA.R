#' Read remind-mfa output
#'
#' Read material flow data exported by the Python model remind-mfa
#' (https://github.com/pik-piam/remind-mfa) when run with its `mrindustry`
#' config (`--config default --config mrindustry --model all`). That run writes
#' one csv file per variable (`Time,Region[,Polymer],<variable>`) into
#' `mrindustry_<model>_<scenario>_eu21/mrindustry/` for every model (steel,
#' cement, plastics) and scenario (SSP1 to SSP5), and then is copied into the 
#' `MFA` source folder.
#'
#' The data is on REMIND region level (eu21 region codes), not on ISO country
#' level, and there is no `convertMFA()`. Always read it with
#' `readSource("MFA", subtype, convert = FALSE)`.
#'
#' @md
#' @param subtype One of
#'   - `steel_production_total`: total steel output after production and forming
#'     losses, before trade (t/yr)
#'   - `steel_production_secondary`: scrap based (EAF) steel output (t/yr)
#'   - `steel_scrap`: home and new scrap plus old scrap after collection and
#'     trade, before surplus is diverted to excess scrap (t/yr)
#'   - `cement_production`: cement output before trade and construction losses
#'     (t/yr)
#'   - `cement_clinker_ratio`: clinker to cement ratio (t clinker / t cement)
#'   - `hvc_production`: high value chemicals production for plastics production
#'     (t/yr)
#'   - `p37_plasticWaste`: plastic waste available for recycling by polymer
#'     (t/yr)
#'
#' @return A [`magpie`][magclass::magclass] object with REMIND regions in the
#'   first dimension, years in the second, and `scenario.variable` (or
#'   `scenario.polymer` for `p37_plasticWaste`) in the third.
#'
#' @author Tobias Diez
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map2
#' @importFrom readr read_csv cols col_character
#'
#' @seealso [`readSource()`]
#' @export
readMFA <- function(subtype) {
  path <- "./"

  layout <- list(
    steel_production_total     = list(model = "steel"),
    steel_production_secondary = list(model = "steel"),
    steel_scrap                = list(model = "steel"),
    cement_production          = list(model = "cement"),
    cement_clinker_ratio       = list(model = "cement"),
    hvc_production             = list(model = "plastics"),
    p37_plasticWaste           = list(model = "plastics", dim3 = "Polymer")
  )

  if (!subtype %in% names(layout)) {
    stop(paste("Invalid subtype -- supported subtypes are:",
               paste(names(layout), collapse = ", ")))
  }

  model <- layout[[subtype]][["model"]]
  dim3 <- layout[[subtype]][["dim3"]]

  # one run folder per scenario: mrindustry_<model>_<scenario>_eu21
  pattern <- paste0("^mrindustry_", model, "_(.+)_eu21$")
  runs <- list.files(path, pattern = pattern)
  if (length(runs) == 0) {
    stop(paste0("No run folders mrindustry_", model, "_<scenario>_eu21 found in ",
                normalizePath(path)))
  }
  scenarios <- sub(pattern, "\\1", runs)
  files <- file.path(path, runs, "mrindustry", paste0(subtype, ".csv"))

  dims <- c("Time", "Region", dim3)
  data <- map2(files, scenarios, function(file, scenario) {
    d <- read_csv(file, col_types = cols(.default = col_character()))
    if (!identical(names(d)[-ncol(d)], dims)) {
      stop(paste0("Unexpected columns in ", file, ": ", paste(names(d), collapse = ", "),
                  " (expected: ", paste(c(dims, "<value>"), collapse = ", "), ")"))
    }
    names(d)[ncol(d)] <- "value"
    mutate(d, scenario = scenario, .after = "Region")
  }) %>%
    bind_rows()

  # by default, use the variable name for the third dimension
  if (is.null(dim3)) {
    data <- mutate(data, variable = subtype, .after = "scenario")
  }
  data[["Time"]] <- as.integer(data[["Time"]])
  data[["value"]] <- as.numeric(data[["value"]])

  x <- as.magpie(data, spatial = "Region", temporal = "Time", tidy = TRUE)
  getSets(x) <- c("region", "year", "scenario", if (is.null(dim3)) "variable" else "polymer")
  # as.magpie NA-fills missing region/year/scenario combinations
  if (anyNA(x)) {
    stop(paste("NA values in", subtype))
  }
  return(x)
}
