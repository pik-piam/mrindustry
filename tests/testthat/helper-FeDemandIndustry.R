# Test harness for calcOutput("FeDemandIndustry") without source data.
#
# All upstream calculations that need source data (UE projections, GDP, IEA energy balances, IEA ETP) are replaced by
# deterministic synthetic data for all 249 countries. The package-internal parameter files
# (inst/extdata/specific_*.csv, fixing_year.csv, ueCorrectionFactors.csv, ...) are used as they are.
# The synthetic data is shaped such that the special cases of calcFeDemandIndustry are exercised:
# - historic zero activity (primary steel in NEN and a few other countries, cement in a few countries)
# - absurdly high specific energy of primary steel in NEN (replaced by EU average)
# - cement specific energy below the thermodynamic limit in JPN (replaced by average of other regions)
# - CHA industry electricity demand below the 21 EJ target in 2025

fdiFixture <- function(...) {
  testthat::test_path("fixtures", "FeDemandIndustry", ...)
}

# scenarios computed by calcFeDemandIndustry, given the current scenario definitions in mrdrivers
fdiScenarios <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "SSP2_lowEn", "SSP2_highDemDEU",
                  "SSP2IndiaHigh", "SSP2IndiaMedium")

# synthetic world ----
fdiWorld <- local({
  world <- NULL
  function() {
    if (is.null(world)) {
      world <<- fdiBuildWorld()
    }
    world
  }
})

fdiBuildWorld <- function() {
  mapping <- utils::read.csv2(fdiFixture("regionmapping_21_EU11.csv"))
  mapping <- mapping[order(mapping$CountryCode, method = "radix"), ]
  countries <- mapping$CountryCode
  regions <- stats::setNames(mapping$RegionCode, countries)
  n <- length(countries)

  subsectors <- c("cement", "chemicals", "otherInd", "steel_primary", "steel_secondary")
  # activity per unit of GDP (Gt/million $ for cement and steel, $tn/million $ for chemicals and otherInd)
  baseIntensity <- c(cement = 3e-8, chemicals = 5e-8, otherInd = 1.5e-7,
                     steel_primary = 1e-8, steel_secondary = 4e-9)
  # specific FE demand (GJ/t for cement and steel, EJ/$tn for chemicals and otherInd)
  baseSpecificEnergy <- c(cement = 3.5, chemicals = 9, otherInd = 3, steel_primary = 18, steel_secondary = 2)
  carriers <- list(cement    = c("feso", "feli", "fega", "feel"),
                   chemicals = c("feso", "feli", "fega", "feelwlth"),
                   steel     = c("feso", "feli", "fega", "feel"),
                   otherInd  = c("feso", "feli", "fega", "fehe", "feelwlth"))

  withr::with_seed(42, {
    gdp2020 <- exp(stats::runif(n, log(5e2), log(5e6)))
    growthHist <- stats::runif(n, 0, 0.05)
    growthFuture <- stats::runif(n, 0.005, 0.04)
    intensity <- outer(rep(1, n), baseIntensity) * exp(matrix(stats::runif(n * 5, -1, 1), n))
    declineHist <- matrix(stats::runif(n * 5, -0.01, 0.01), n)
    declineFuture <- matrix(stats::runif(n * 5, 0, 0.015), n)
    specificEnergy <- outer(rep(1, n), baseSpecificEnergy) * exp(matrix(stats::runif(n * 5, -0.3, 0.3), n))
    carrierMix <- lapply(carriers, function(fe) {
      x <- matrix(-log(stats::runif(n * length(fe))), n, dimnames = list(NULL, fe))
      x / rowSums(x)
    })
    carrierTrend <- lapply(carriers, function(fe) {
      matrix(stats::runif(n * length(fe), -0.5, 0.5), n, dimnames = list(NULL, fe))
    })
    bioShare <- stats::runif(n, 0, 0.3)
    fePhase <- stats::runif(n, 0, 2 * pi)
    zeroSteel <- sample(setdiff(countries, names(regions)[regions == "NEN"]), 10)
    zeroCement <- sample(countries, 5)
  }, .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
  dimnames(intensity) <- dimnames(declineHist) <- dimnames(declineFuture) <- dimnames(specificEnergy) <-
    list(countries, subsectors)

  # big economies [million $]; CHN dominates cement and steel production
  gdp2020[match(c("CHN", "USA", "IND", "JPN", "DEU", "RUS", "IDN", "BRA", "FRA", "GBR"), countries)] <-
    c(2.4e7, 2e7, 8.9e6, 5.2e6, 4.4e6, 4.1e6, 3.2e6, 3.1e6, 3e6, 3e6)
  intensity["CHN", ] <- baseIntensity * c(3, 1, 1, 3, 1)

  # special cases
  zeroHistory <- rbind(
    data.frame(iso3c = c(names(regions)[regions == "NEN"], zeroSteel), subsector = "steel_primary", until = 2010),
    data.frame(iso3c = zeroCement, subsector = "cement", until = 2000)
  )
  specificEnergy[regions == "NEN", c("steel_primary", "steel_secondary")] <- 300
  specificEnergy["JPN", "cement"] <- 1

  list(countries = countries, regions = regions, subsectors = subsectors, carriers = carriers,
       gdp2020 = stats::setNames(gdp2020, countries), growthHist = growthHist, growthFuture = growthFuture,
       intensity = intensity, declineHist = declineHist, declineFuture = declineFuture,
       specificEnergy = specificEnergy, carrierMix = carrierMix, carrierTrend = carrierTrend,
       bioShare = bioShare, fePhase = fePhase, zeroHistory = zeroHistory)
}

fdiScenarioParameters <- function(scenario) {
  parameters <- list(SSP1 = c(growth = 1.05, decline = 1.3),
                     SSP2 = c(growth = 1, decline = 1),
                     SSP3 = c(growth = 0.7, decline = 0.6),
                     SSP4 = c(growth = 0.85, decline = 0.9),
                     SSP5 = c(growth = 1.25, decline = 0.8),
                     SSP2IndiaHigh = c(growth = 1.02, decline = 1),
                     SSP2IndiaMedium = c(growth = 1.01, decline = 1))
  if (is.null(parameters[[scenario]])) {
    return(c(growth = 1, decline = 1))
  }
  parameters[[scenario]]
}

# GDP [million $] as country x year matrix; scenario-independent until 2020
fdiGdpMatrix <- function(world, scenario, years) {
  p <- fdiScenarioParameters(scenario)
  dt <- outer(rep(1, length(world$countries)), years - 2020)
  logGrowth <- ifelse(dt <= 0,
                      world$growthHist * dt,
                      world$growthFuture * p[["growth"]] * 60 * (1 - exp(-dt / 60)))
  matrix(world$gdp2020 * exp(logGrowth), ncol = length(years), dimnames = list(world$countries, years))
}

# activity as country x year matrix; scenario-independent until 2020
fdiUeMatrix <- function(world, scenario, subsector, years, zeroHistory = TRUE) {
  p <- fdiScenarioParameters(scenario)
  dt <- outer(rep(1, length(world$countries)), years - 2020)
  rate <- ifelse(dt <= 0, world$declineHist[, subsector], world$declineFuture[, subsector] * p[["decline"]])
  x <- fdiGdpMatrix(world, scenario, years) * world$intensity[, subsector] * exp(-rate * dt)
  if (zeroHistory) {
    zero <- world$zeroHistory[world$zeroHistory$subsector == subsector, ]
    for (i in seq_len(nrow(zero))) {
      x[zero$iso3c[i], years <= zero$until[i]] <- 0
    }
  }
  x
}

# same structure as calcIndustry_Value_Added()/calcSteel_Projections() output
fdiUe <- function(world, scenarios, subsectors, years = 1993:2150) {
  d <- do.call(rbind, lapply(scenarios, function(scenario) {
    do.call(rbind, lapply(subsectors, function(subsector) {
      m <- fdiUeMatrix(world, scenario, subsector, years)
      data.frame(scenario = scenario, iso3c = rep(world$countries, length(years)),
                 pf = paste0("ue_", subsector), year = rep(years, each = length(world$countries)),
                 value = as.vector(m))
    }))
  }))
  as.magpie(d, spatial = 2, temporal = 4, datacol = 5)
}

# same structure as mrdrivers::calcGDP() output
fdiGdp <- function(world, scenarios, years = 1993:2150) {
  a <- array(unlist(lapply(scenarios, function(s) fdiGdpMatrix(world, s, years))),
             dim = c(length(world$countries), length(years), length(scenarios)),
             dimnames = list(iso3c = world$countries, year = paste0("y", years), variable = scenarios))
  as.magpie(a, spatial = 1, temporal = 2)
}

# same structure as calcEnergyBalancesOutputToIndustry() output [EJ]
fdiEnergyBalances <- function(world, years = 1990:2022) {
  # FE is reported even for historic years without reported activity
  fe <- lapply(stats::setNames(nm = world$subsectors), function(s) {
    fdiUeMatrix(world, "SSP2", s, years, zeroHistory = FALSE) * world$specificEnergy[, s]
  })
  fe$steel <- fe$steel_primary + fe$steel_secondary
  fe$steel_primary <- fe$steel_secondary <- NULL

  dt <- outer(rep(1, length(world$countries)), (years - 2005) / 100)
  noise <- 1 + 0.03 * sin(outer(world$fePhase, years, "+"))
  items <- list()
  for (s in names(world$carriers)) {
    mix <- lapply(stats::setNames(nm = world$carriers[[s]]), function(fe) {
      world$carrierMix[[s]][, fe] * (1 + world$carrierTrend[[s]][, fe] * dt)
    })
    total <- Reduce(`+`, mix)
    for (carrier in names(mix)) {
      value <- fe[[s]] * noise * mix[[carrier]] / total
      se <- switch(substr(carrier, 1, 4), feso = "seso", feli = "seliq", fega = "sega", fehe = "sehe", feel = "seel")
      td <- sub("^fe", "td", carrier)
      if (se %in% c("seso", "seliq", "sega")) {
        items[[paste0(se, "fos.", carrier, "_", s, ".", td, "_", s)]] <- value * (1 - world$bioShare)
        items[[paste0(se, "bio.", carrier, "_", s, ".", td, "_", s)]] <- value * world$bioShare
      } else {
        items[[paste0(se, ".", carrier, "_", s, ".", td, "_", s)]] <- value
      }
    }
  }
  # items not belonging to industry
  items[["seel.feelb.tdelb"]] <- fe$otherInd
  items[["segafos.fegab.tdgab"]] <- fe$chemicals

  a <- array(unlist(items), dim = c(length(world$countries), length(years), length(items)),
             dimnames = list(region = world$countries, year = paste0("y", years), data = names(items)))
  as.magpie(a, spatial = 1, temporal = 2)
}

# same structure as readSource("IEA_ETP", "industry", convert = FALSE) [PJ]
fdiIeaEtp <- function() {
  years <- c(2014, seq(2025, 2060, 5))
  subsectors <- c(cement    = "Cement - final energy consumption",
                  chemicals = "Chemicals and petrochemicals - final energy consumption and chemical feedstock",
                  steel     = "Iron and steel - final energy consumption incl. blast furnaces and coke ovens",
                  paper     = "Pulp and paper - final energy consumption")
  carriers <- c("Coal", "Oil", "Natural gas", "Electricity", "Heat", "Biomass", "Waste", "Other renewables")
  grid <- expand.grid(region = c("WORLD", "OECD", "Non-OECD"), year = years, scenario = c("RTS", "2DS"),
                      subsector = names(subsectors), carrier = carriers, stringsAsFactors = FALSE)
  base <- 1000 * (1 + 0.5 * match(grid$carrier, carriers)) * (1 + match(grid$subsector, names(subsectors))) *
    ifelse(grid$region == "OECD", 0.6, 1) * ifelse(grid$scenario == "2DS", 0.8, 1)
  trend <- 1 + (grid$year - 2014) / 100 *
    ifelse(grid$carrier %in% c("Electricity", "Natural gas", "Biomass"), 1.5, -0.5)
  grid$value <- ifelse(grid$carrier == "Heat" & grid$subsector != "paper", 0, base * trend)
  total <- stats::aggregate(value ~ region + year + scenario + carrier, grid, sum)
  total$value <- total$value * 1.6 # otherInd = total - (cement + chemicals + steel) includes paper and the rest
  total$subsector <- "total"
  grid <- rbind(grid, total[, names(grid)])
  grid <- rbind(grid,
                transform(stats::aggregate(value ~ region + year + scenario + subsector, grid, sum),
                          carrier = "Total")[, names(grid)])

  grid$variable <- paste0("Industry|", ifelse(grid$subsector == "total", "Total industry final energy consumption",
                                              subsectors[grid$subsector]), "|", grid$carrier)
  # faulty data in the original source
  faulty <- grid$region == "Non-OECD" & grid$year == 2055 & grid$subsector == "chemicals" & grid$carrier == "Oil"
  grid$value[faulty] <- grid$value[faulty] * 10

  grid$unit <- "PJ"
  x <- as.magpie(tibble::as_tibble(grid[, c("region", "year", "scenario", "variable", "unit", "value")]),
                 spatial = 1, temporal = 2, tidy = TRUE)
  # mirrors readIEA_ETP()
  x[, 2055, "RTS.Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|",
    pmatch = TRUE]["OECD", , ] <- NA
  x
}

# mocks ----
fdiCheckArgs <- function(type, args, expected, optional = character()) {
  unexpected <- setdiff(names(args), c(names(expected), optional))
  missing <- setdiff(names(expected), names(args))
  wrong <- Filter(function(a) !identical(args[[a]], expected[[a]]), intersect(names(expected), names(args)))
  if (length(c(unexpected, missing, wrong)) > 0) {
    problems <- c(if (length(unexpected) > 0) paste("unexpected arguments", toString(unexpected)),
                  if (length(missing) > 0) paste("missing arguments", toString(missing)),
                  if (length(wrong) > 0) paste("unexpected values for", toString(wrong)))
    base::stop("Unexpected call of calcOutput/readSource(\"", type, "\") in test mock: ",
               paste(problems, collapse = "; "), call. = FALSE)
  }
}

# mimic what calcOutput() does with the return value of the calc function for aggregate = FALSE
fdiCalcOutputResult <- function(x, args) {
  if (!is.null(args$years)) {
    stopifnot(all(args$years %in% getYears(x, as.integer = TRUE)))
    x <- x[, args$years, ]
  }
  if (isTRUE(args$supplementary)) {
    return(list(x = x, weight = NULL, unit = "", description = ""))
  }
  x
}

localFeDemandIndustryMocks <- function(.env = parent.frame()) {
  world <- fdiWorld()
  ueArgs <- list(match.steel.historic.values = TRUE, match.steel.estimates = "IEA_ETP", aggregate = FALSE)

  fakeCalcOutput <- function(type, ...) {
    args <- list(...)
    x <- switch(
      type,
      Industry_Value_Added = {
        fdiCheckArgs(type, args, ueArgs, optional = c("scenarios", "years", "warnNA"))
        fdiUe(world, args$scenarios, c("cement", "chemicals", "otherInd"))
      },
      Steel_Projections = {
        fdiCheckArgs(type, args, c(list(subtype = "production"), ueArgs),
                     optional = c("scenarios", "years", "supplementary"))
        fdiUe(world, args$scenarios, c("steel_primary", "steel_secondary"))
      },
      GDP = {
        fdiCheckArgs(type, args, list(average2020 = FALSE, aggregate = FALSE), optional = c("scenario", "years"))
        fdiGdp(world, args$scenario)
      },
      EnergyBalancesOutputToIndustry = {
        fdiCheckArgs(type, args, list(aggregate = FALSE))
        fdiEnergyBalances(world)
      },
      return(madrat::calcOutput(type, ...))
    )
    fdiCalcOutputResult(x, args)
  }

  fakeReadSource <- function(type, subtype = NULL, subset = NULL, convert = TRUE, supplementary = FALSE) {
    if (type == "IEA_ETP") {
      fdiCheckArgs(type, list(subtype = subtype, convert = convert), list(subtype = "industry", convert = FALSE))
      return(fdiIeaEtp())
    }
    madrat::readSource(type = type, subtype = subtype, subset = subset, convert = convert,
                       supplementary = supplementary)
  }

  testthat::local_mocked_bindings(
    calcOutput = fakeCalcOutput,
    readSource = fakeReadSource,
    toolGetIEAYear = function(ieaVersion) 2024,
    .env = .env
  )
}

# isolated madrat setup in a temporary main folder (never touches the user's madrat folders)
localMadratTestConfig <- function(.env = parent.frame()) {
  mainfolder <- withr::local_tempdir(pattern = "madrat", .local_envir = .env)
  mappingfolder <- file.path(mainfolder, "mappings")
  dir.create(file.path(mappingfolder, "regional"), recursive = TRUE)
  file.copy(fdiFixture(c("regionmapping_21_EU11.csv", "regionmappingOECD.csv")),
            file.path(mappingfolder, "regional"))
  sourcefolder <- file.path(mainfolder, "sources")
  # readSource() requires the source folder to exist, even if the data is read from inst/extdata
  dir.create(file.path(sourcefolder, "industry_subsectors_specific"), recursive = TRUE)

  madrat::setConfig(mainfolder = mainfolder,
                    sourcefolder = sourcefolder,
                    cachefolder = file.path(mainfolder, "cache"),
                    mappingfolder = mappingfolder,
                    outputfolder = file.path(mainfolder, "output"),
                    pucfolder = file.path(mainfolder, "puc"),
                    tmpfolder = file.path(mainfolder, "tmp"),
                    globalenv = FALSE,
                    forcecache = FALSE,
                    ignorecache = NULL,
                    diagnostics = FALSE,
                    debug = FALSE,
                    verbosity = 0,
                    .verbose = FALSE,
                    .local = .env)
}

# madrat's cache fingerprinting scans all madrat packages and warns about mappings missing in the temporary
# mapping folder and about functions outside the scope of the attached packages; irrelevant for the tests
fdiMuffleFingerprintWarnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("^Mapping in .* not found!|could not be found in the scope of packages|could not be identified",
              conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

# run calcOutput("FeDemandIndustry") on synthetic data (memoised, as one run takes about half a minute)
runFeDemandIndustry <- local({
  results <- list()
  function(scenarios) {
    key <- paste(scenarios, collapse = ",")
    if (is.null(results[[key]])) {
      localMadratTestConfig()
      localFeDemandIndustryMocks()
      # testthat turns every lifecycle deprecation into a warning, and calcFeDemandIndustry() calls the deprecated
      # case_when() with scalar conditions (re-normalising IEA ETP shares) ~20,000 times, which slows the run down
      # five-fold; outside of tests, the deprecation is only reported once per session
      withr::local_options(lifecycle_verbosity = "quiet")
      results[[key]] <<- fdiMuffleFingerprintWarnings(
        madrat::calcOutput("FeDemandIndustry", scenarios = scenarios, aggregate = FALSE, supplementary = TRUE)
      )
    }
    results[[key]]
  }
})

# regression snapshots ----
# global totals per scenario, item and year
fdiGlobalTotals <- function(x) {
  totals <- t(as.array(dimSums(x, dim = 1))[1, , ])
  totals <- totals[order(rownames(totals), method = "radix"), ]
  colnames(totals) <- sub("^y", "", colnames(totals))
  data.frame(scenario = sub("\\..*$", "", rownames(totals)), item = sub("^[^.]*\\.", "", rownames(totals)),
             signif(totals, 12), check.names = FALSE, row.names = NULL)
}

# per country and scenario, a weighted sum over all items and years, so that any changed value shows up in the
# fingerprint of its country, including swaps between years or items
fdiCountryFingerprints <- function(x) {
  scenarios <- sort(getItems(x, "scenario"), method = "radix")
  items <- sort(getItems(x, "item"), method = "radix")
  weights <- withr::with_seed(1, stats::runif(length(items) * nyears(x), 0.5, 1.5),
                              .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
                              .rng_sample_kind = "Rejection")
  weights <- matrix(weights, nyears(x), length(items))
  a <- as.array(x)
  fingerprints <- vapply(scenarios, function(scenario) {
    rowSums(sweep(a[, , paste0(scenario, ".", items)], 2:3, weights, "*"), dims = 1)
  }, numeric(dim(a)[1]))
  fingerprints <- fingerprints[order(rownames(fingerprints), method = "radix"), , drop = FALSE]
  data.frame(iso3c = rownames(fingerprints), signif(fingerprints, 12), check.names = FALSE, row.names = NULL)
}

fdiWriteCsv <- function(data, .env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = .env)
  utils::write.csv(data, path, row.names = FALSE)
  path
}

# compare snapshot csv files element-wise with a relative tolerance, ignoring floating point noise
fdiCompareCsv <- function(old, new, tolerance = 1e-9) {
  old <- utils::read.csv(old, check.names = FALSE)
  new <- utils::read.csv(new, check.names = FALSE)
  if (!identical(dim(old), dim(new)) || !identical(names(old), names(new))) {
    return(FALSE)
  }
  numeric <- vapply(old, is.numeric, logical(1))
  if (!identical(numeric, vapply(new, is.numeric, logical(1))) || !identical(old[!numeric], new[!numeric])) {
    return(FALSE)
  }
  old <- as.matrix(old[numeric])
  new <- as.matrix(new[numeric])
  if (!identical(is.na(old), is.na(new))) {
    return(FALSE)
  }
  relativeDifference <- abs(old - new) / pmax(abs(old), abs(new), 1e-12)
  all(relativeDifference[!is.na(relativeDifference)] <= tolerance)
}
