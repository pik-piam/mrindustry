version <- readLines(system.file("extdata", "MFA_version.txt", package = "mrindustry"))
countries <- madrat::getISOlist()

# write <mfa>/<folder>/<run>/<file> as the mrindustry export of remind-mfa
writeRun <- function(mfa, run, file, lines, folder = version) {
  dir <- file.path(mfa, folder, run)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file.path(dir, file))
}

writeSteel <- function(mfa, scenario, value, folder = version) {
  writeRun(mfa, paste0("steel_", scenario, "_iso249"), "steel_scrap.csv",
           c("Time,Region,steel_scrap", paste0("2000,", countries, ",", value)), folder = folder)
}

writePlastics <- function(mfa) {
  writeRun(mfa, "plastics_SSP2_eu21", "hvc_production.csv",
           c("Time,Region,hvc_production", "2000,EWN,1", "2000,ESW,2"))
  writeRun(mfa, "plastics_SSP2_eu21", "p37_plasticWaste.csv",
           c("Time,Region,Polymer,p37_plasticWaste",
             "2000,EWN,PET,1", "2000,EWN,Other thermoplastics,2",
             "2000,ESW,PET,3", "2000,ESW,Other thermoplastics,4"))
}

readMFAFrom <- function(subtype, mfa, convert = FALSE) {
  madrat::localConfig(mainfolder = withr::local_tempdir(), verbosity = 0, .verbose = FALSE)
  madrat::redirectSource("MFA", target = mfa)
  withCallingHandlers(
    madrat::readSource("MFA", subtype = subtype, convert = convert),
    warning = function(w) {
      # madrat's dependency analysis warns about stuff that is not relevant here
      if (grepl("^Mapping in .* not found!$|could not be found in the scope of packages", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

test_that("readMFA reads each scenario run into region x year x scenario.variable", {
  mfa <- withr::local_tempdir()
  writeSteel(mfa, "SSP2", 1)
  writeSteel(mfa, "SSP2_lowEn", 2)

  x <- readMFAFrom("steel_scrap", mfa)

  expect_equal(unname(magclass::getSets(x)), c("region", "year", "scenario", "variable"))
  expect_setequal(magclass::getItems(x, dim = 3), c("SSP2.steel_scrap", "SSP2_lowEn.steel_scrap"))
  expect_true(all(x[, , "SSP2_lowEn"] == 2))
})

test_that("readMFA ignores runs of other models and of other versions", {
  mfa <- withr::local_tempdir()
  writeSteel(mfa, "SSP2", 1)
  writeSteel(mfa, "SSP5", 5, folder = "1.0.0-00000000")
  writePlastics(mfa)

  x <- readMFAFrom("steel_scrap", mfa)

  expect_equal(magclass::getItems(x, dim = "scenario"), "SSP2")
})

test_that("readMFA reads the polymers of plastics_waste into the third dimension", {
  mfa <- withr::local_tempdir()
  writePlastics(mfa)

  x <- readMFAFrom("plastics_waste", mfa)

  expect_equal(unname(magclass::getSets(x)), c("region", "year", "scenario", "polymer"))
  expect_equal(as.vector(x["EWN", , "SSP2.Other thermoplastics"]), 2)
})

test_that("readMFA fails if the pinned version has no run of the model", {
  mfa <- withr::local_tempdir()
  writeSteel(mfa, "SSP2", 1, folder = "1.0.0-00000000")

  expect_error(readMFAFrom("steel_scrap", mfa), "No run folders steel_<scenario>_iso249")
})

test_that("readMFA fails on unexpected columns", {
  mfa <- withr::local_tempdir()
  writeRun(mfa, "steel_SSP2_iso249", "steel_scrap.csv", c("Time,Region,Polymer,steel_scrap", "2000,DEU,PET,1"))

  expect_error(readMFAFrom("steel_scrap", mfa), "Unexpected columns")
})

test_that("readMFA fails on missing values", {
  mfa <- withr::local_tempdir()
  writeRun(mfa, "steel_SSP2_iso249", "steel_scrap.csv", c("Time,Region,steel_scrap", "2000,DEU,", "2000,FRA,2"))

  expect_error(readMFAFrom("steel_scrap", mfa), "NA values")
})

test_that("convertMFA keeps steel unchanged", {
  mfa <- withr::local_tempdir()
  writeSteel(mfa, "SSP2", 1)

  x <- readMFAFrom("steel_scrap", mfa, convert = TRUE)
  raw <- readMFAFrom("steel_scrap", mfa)

  expect_identical(dimnames(x[, , "SSP2"]), dimnames(raw))
  expect_equal(as.vector(x[, , "SSP2"]), as.vector(raw))
})

test_that("convertMFA gives the SSP2 variants that remind-mfa does not run the SSP2 data", {
  mfa <- withr::local_tempdir()
  writeSteel(mfa, "SSP2", 1)

  x <- readMFAFrom("steel_scrap", mfa, convert = TRUE)

  expect_setequal(magclass::getItems(x, dim = "scenario"),
                  c("SSP2", "SSP2_lowEn", "SSP2_highDemDEU", "SSP2IndiaHigh", "SSP2IndiaMedium"))
  expect_true(all(x == 1))
})

test_that("convertMFA keeps the data of SSP2 variants that remind-mfa runs", {
  mfa <- withr::local_tempdir()
  writeSteel(mfa, "SSP2", 1)
  writeSteel(mfa, "SSP2_lowEn", 2)

  x <- readMFAFrom("steel_scrap", mfa, convert = TRUE)

  expect_true(all(x[, , "SSP2_lowEn"] == 2))
})
