#' Read remind-mfa output
#'
#' Read material flow data exported by the Python model remind-mfa
#' (https://github.com/pik-piam/remind-mfa) when run with its `mrindustry`
#' config (`--config default --config mrindustry --model all`). That run
#' yields one csv file per variable for every model (steel, cement, plastics)
#' and scenario in `MFA/<version>/<model>_<scenario>_<regionmapping>/`, where
#' `<version>` is the remind-mfa version and commit, e.g. `1.0.1-b2ef4346`.
#' The version to read is set in `inst/extdata/MFA_version.txt`.
#'
#' @md
#' @param subtype One of
#'   - `steel_production_total`: total steel output after production and forming
#'     losses, before trade (t/yr)
#'   - `steel_production_secondary`: scrap-based (EAF) steel output (t/yr)
#'   - `steel_scrap`: home and new scrap plus old scrap after collection and
#'     trade, before surplus is diverted to excess scrap (t/yr)
#'   - `cement_production`: cement output before trade and construction losses
#'     (t/yr)
#'   - `cement_clinker_ratio`: clinker to cement ratio (t clinker / t cement)
#'   - `plastics_hvc_production`: high-value chemicals production for plastics
#'     production (t/yr)
#'   - `plastics_waste`: plastic waste available for recycling, by polymer
#'     (t/yr)
#'
#' @return A [`magpie`][magclass::magclass] object with ISO countries (steel
#'   and cement) or eu21 regions (plastics) in the first dimension, years in the
#'   second, and `scenario.variable` (or `scenario.polymer` for
#'   `plastics_waste`) in the third.
#'
#' @author Tobias Diez
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map2
#' @importFrom readr read_csv cols col_character
#'
#' @seealso [readSource()], [convertMFA()]
#' @export
readMFA <- function(subtype) {
  path <- "./"
  version <- readLines(toolGetMapping("MFA_version.txt", where = "mrindustry", returnPathOnly = TRUE))

  layout <- list(
    steel_production_total     = list(model = "steel", regionmapping = "iso249"),
    steel_production_secondary = list(model = "steel", regionmapping = "iso249"),
    steel_scrap                = list(model = "steel", regionmapping = "iso249"),
    cement_production          = list(model = "cement", regionmapping = "iso249"),
    cement_clinker_ratio       = list(model = "cement", regionmapping = "iso249"),
    plastics_hvc_production    = list(model = "plastics", regionmapping = "eu21", file = "hvc_production"),
    plastics_waste             = list(model = "plastics", regionmapping = "eu21", file = "p37_plasticWaste",
                                      dim3 = "Polymer")
  )

  if (!subtype %in% names(layout)) {
    stop(paste("Invalid subtype -- supported subtypes are:",
               paste(names(layout), collapse = ", ")))
  }

  model <- layout[[subtype]][["model"]]
  regionmapping <- layout[[subtype]][["regionmapping"]]
  dim3 <- layout[[subtype]][["dim3"]]
  file <- layout[[subtype]][["file"]]
  if (is.null(file)) {
    file <- subtype
  }

  # one run folder per scenario: <version>/<model>_<scenario>_<regionmapping>
  versionPath <- file.path(path, version)
  pattern <- paste0("^", model, "_(.+)_", regionmapping, "$")
  runs <- list.files(versionPath, pattern = pattern)
  if (length(runs) == 0) {
    stop(paste0("No run folders ", model, "_<scenario>_", regionmapping, " found in ",
                file.path(normalizePath(path), version)))
  }
  scenarios <- sub(pattern, "\\1", runs)
  files <- file.path(versionPath, runs, paste0(file, ".csv"))

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
