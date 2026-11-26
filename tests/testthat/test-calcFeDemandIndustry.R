# calcOutput("FeDemandIndustry") runs on synthetic upstream data, see helper-FeDemandIndustry.R

fdiItems <- c(
  "feso_cement", "feli_cement", "fega_cement", "feh2_cement", "feel_cement",
  "feso_chemicals", "feli_chemicals", "fega_chemicals", "feh2_chemicals", "feelhth_chemicals", "feelwlth_chemicals",
  "feso_steel", "feli_steel", "fega_steel", "feh2_steel", "feel_steel_primary", "feel_steel_secondary",
  "feso_otherInd", "feli_otherInd", "fega_otherInd", "feh2_otherInd", "fehe_otherInd", "feelhth_otherInd",
  "feelwlth_otherInd",
  "ue_cement", "ue_chemicals", "ue_otherInd", "ue_steel_primary", "ue_steel_secondary"
)

fdiAllScenarios <- c(fdiScenarios, "SSP2_NAV_all")

fdiArray <- function(x) {
  a <- as.array(x)
  attr(a, "comment") <- NULL
  a
}

test_that("unavailable scenarios are rejected", {
  localMadratTestConfig()
  localFeDemandIndustryMocks()
  expect_error(
    suppressMessages(fdiMuffleFingerprintWarnings(
      madrat::calcOutput("FeDemandIndustry", scenarios = c("SSP2", "SSP7"), aggregate = FALSE)
    )),
    "SSP7 scenario(s) are not available",
    fixed = TRUE
  )
})

test_that("output covers all countries, REMIND years, scenarios and items", {
  result <- runFeDemandIndustry(fdiAllScenarios)
  x <- result$x

  expect_setequal(getItems(x, 1), fdiWorld()$countries)
  expect_identical(getYears(x, as.integer = TRUE), seq(1995L, 2150L, 5L))
  expect_setequal(getItems(x, "scenario"), fdiAllScenarios)
  expect_setequal(getItems(x, "item"), fdiItems)
  expect_identical(ndata(x), length(fdiAllScenarios) * length(fdiItems))
  expect_false(anyNA(x))
  expect_true(all(x >= 0))
  expect_null(result$weight)
})

test_that("SSP2_NAV_all is a copy of SSP2", {
  x <- runFeDemandIndustry(fdiAllScenarios)$x
  expect_identical(fdiArray(setItems(x[, , "SSP2_NAV_all"], "scenario", "SSP2")), fdiArray(x[, , "SSP2"]))
})

test_that("results do not depend on the other requested scenarios", {
  # SSP2_lowEn and SSP5 are derived from SSP2, which is not requested here
  scenarios <- c("SSP2_lowEn", "SSP5")
  subset <- runFeDemandIndustry(scenarios)$x
  full <- runFeDemandIndustry(fdiAllScenarios)$x

  expect_setequal(getItems(subset, "scenario"), scenarios)
  expect_equal(fdiArray(subset), fdiArray(full[, , getItems(subset, 3)]), tolerance = 1e-12)
})

test_that("UE of SSP2 follows the activity projections, corrected for recent trends", {
  x <- runFeDemandIndustry(fdiAllScenarios)$x
  world <- fdiWorld()
  years <- seq(1995, 2150, 5)
  corrections <- utils::read.csv(system.file("extdata", "ueCorrectionFactors.csv", package = "mrindustry"),
                                 sep = ";")

  for (subsector in world$subsectors) {
    input <- fdiUeMatrix(world, "SSP2", subsector, years)
    correction <- corrections[match(world$regions, corrections$region), paste0("ue_", subsector)]
    expected <- input * outer(correction, years >= 2025) + input * outer(rep(1, nrow(input)), years < 2025)
    actual <- fdiArray(x[world$countries, years, paste0("SSP2.ue_", subsector)])[, , 1]
    expect_equal(actual[input > 0], expected[input > 0], tolerance = 1e-12, label = subsector)

    # historic zero activity is replaced by the first non-zero activity per unit of GDP
    gdp <- fdiGdpMatrix(world, "SSP2", years)
    for (iso3c in rownames(input)[apply(input == 0, 1, any)]) {
      first <- which(input[iso3c, ] > 0)[1]
      zero <- input[iso3c, ] == 0
      backfilled <- gdp[iso3c, zero] * input[iso3c, first] / gdp[iso3c, first]
      expect_equal(unname(actual[iso3c, zero]), unname(backfilled), tolerance = 1e-12,
                   label = paste(subsector, iso3c))
    }
  }
})

test_that("results on synthetic data are unchanged", {
  # Regression test for refactorings. If results change on purpose, review and accept the new snapshots with
  # testthat::snapshot_review("calcFeDemandIndustry/").
  x <- runFeDemandIndustry(fdiAllScenarios)$x[, , fdiScenarios]
  expect_snapshot_file(fdiWriteCsv(fdiGlobalTotals(x)), "global-totals.csv", compare = fdiCompareCsv)
  expect_snapshot_file(fdiWriteCsv(fdiCountryFingerprints(x)), "country-fingerprints.csv", compare = fdiCompareCsv)
})
