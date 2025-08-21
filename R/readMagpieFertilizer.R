#' readMagpieFertilizer
#'
#' Read function for fertilizer data coming from the MAgPIE model.
#'
#' @author Leonie Schweiger
#' @seealso [readSource()]
#' \dontrun{
#'   a <- readSource(type = "MagpieFertilizer")
#' }
readMagpieFertilizer <- function() {
  
  gdx <- "fulldata.gdx"
  stopifnot(file.exists(gdx))

  x1 <- magpie4::reportNitrogenBudgetPasture(gdx)[,,"Resources|Nitrogen|Pasture Budget|Inputs|+|Fertilizer (Mt Nr/yr)"]
  x2 <- magpie4::reportNitrogenBudgetCropland(gdx)[,,"Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer (Mt Nr/yr)"]
  x <- x1 + x2
  getItems(x, dim=3) <- c("Fertilizer Input (Mt Nr/yr)")
  
  return(x)
}