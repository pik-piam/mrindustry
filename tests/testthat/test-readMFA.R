# write <dir>/mrindustry_<model>_<scenario>_eu21/mrindustry/<file> as remind-mfa does
writeRun <- function(dir, model, scenario, file, lines) {
  run <- file.path(dir, paste0("mrindustry_", model, "_", scenario, "_eu21"), "mrindustry")
  dir.create(run, recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file.path(run, file))
}

test_that("readMFA combines scenarios into region x year x scenario.variable", {
  dir <- withr::local_tempdir()
  writeRun(dir, "steel", "SSP1", "steel_scrap.csv",
           c("Time,Region,steel_scrap", "2000,EUR,1.5", "2000,USA,2.5", "2005,EUR,3", "2005,USA,4"))
  writeRun(dir, "steel", "SSP2", "steel_scrap.csv",
           c("Time,Region,steel_scrap", "2000,EUR,10", "2000,USA,20", "2005,EUR,30", "2005,USA,40"))
  # a run of another model is not picked up for a steel subtype
  writeRun(dir, "cement", "SSP2", "cement_production.csv",
           c("Time,Region,cement_production", "2000,EUR,1", "2000,USA,2"))
  withr::local_dir(dir)

  x <- readMFA("steel_scrap")

  expect_true(magclass::is.magpie(x))
  expect_equal(dim(x), c(2, 2, 2))
  expect_equal(unname(magclass::getSets(x)), c("region", "year", "scenario", "variable"))
  expect_setequal(magclass::getItems(x, dim = 1), c("EUR", "USA"))
  expect_equal(magclass::getItems(x, dim = 2), c("y2000", "y2005"))
  expect_setequal(magclass::getItems(x, dim = 3), c("SSP1.steel_scrap", "SSP2.steel_scrap"))
  expect_equal(as.vector(x["USA", "y2005", "SSP1.steel_scrap"]), 4)
  expect_equal(as.vector(x["EUR", "y2000", "SSP2.steel_scrap"]), 10)
})

test_that("readMFA reads p37_plasticWaste into region x year x scenario.polymer", {
  dir <- withr::local_tempdir()
  writeRun(dir, "plastics", "SSP2", "p37_plasticWaste.csv",
           c("Time,Region,Polymer,p37_plasticWaste",
             "2000,EUR,PET,1", "2000,EUR,Other thermoplastics,2",
             "2000,USA,PET,3", "2000,USA,Other thermoplastics,4"))
  withr::local_dir(dir)

  x <- readMFA("p37_plasticWaste")

  expect_equal(dim(x), c(2, 1, 2))
  expect_equal(unname(magclass::getSets(x)), c("region", "year", "scenario", "polymer"))
  expect_setequal(magclass::getItems(x, dim = 3), c("SSP2.PET", "SSP2.Other thermoplastics"))
  expect_equal(as.vector(x["USA", , "SSP2.Other thermoplastics"]), 4)
})

test_that("readMFA fails clearly on missing runs, unexpected columns and NA", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_error(readMFA("cement_production"), "No run folders")

  writeRun(dir, "cement", "SSP2", "cement_production.csv",
           c("Time,Region,Polymer,cement_production", "2000,EUR,PET,1"))
  expect_error(readMFA("cement_production"), "Unexpected columns")

  writeRun(dir, "cement", "SSP2", "cement_production.csv",
           c("Time,Region,cement_production", "2000,EUR,", "2000,USA,2"))
  expect_error(readMFA("cement_production"), "NA values")
})
