test_that("inst/mrindustry_sources.csv lists the current sources of all calc functions", {
  stored <- utils::read.csv(system.file("mrindustry_sources.csv", package = "mrindustry"),
                            colClasses = "character")
  expect_equal(stored, getSources_mrindustry(),
               info = "Update the list with getSources_mrindustry(\"inst/mrindustry_sources.csv\").")
})
