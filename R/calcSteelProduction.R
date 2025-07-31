#' Get Steel Production data
#' 
#' @description
#' Calc steel production from WorldSteel datasets. Can be aggregated to regions 
#' via calcOutput aggregate parameter. Uses 
#' \link[readWorldSteelDigitised]{WorldSteelDigitised} and 
#' \link[readWorldSteelDatabase]{WorldSteelDatabase} datasets, the former for
#' historic, the latter for current data.
#' @author Merlin Jo Hosak
#' @return Steel Production across all regions from 1900-2022 as magpie within 
#' list of metadata (in calcOutput format).
#' @export
calcSteelProduction <- function() {
  data <- getSteelProductionData()
  
  # Interpolate
  data$recent <- toolInterpolate(data$recent)
  data$current <- toolInterpolate(data$current)
  
  # Extrapolate
  prod <- extrapolateSteelProduction(data)
  
  # Finalize for calcOutput
  prod[is.na(prod)] <- 0 ## fill remaining NA with zero # TODO: check
  
  if (any(is.na(prod))) { ## check if there are any NA left in prod
    warning("There are still NA values in the production data after extrapolation.")
  }
  
  result <- list(x = prod, 
                 weight = NULL,
                 unit='Tonnes',
                 description='Steel production from 1900-2100 yearly for the SIMSON format')
  
  return(result)
}

getSteelProductionData <- function() {
  # load data
  prod_hist <- readSource('WorldSteelDigitised', subtype='world_production',convert=F)
  prod_recent <- readSource('WorldSteelDigitised', subtype='production')
  prod_current <- readSource('WorldSteelDatabase', subtype='production')
  
  return(list(hist=prod_hist, 
              recent=prod_recent, 
              current=prod_current))
}

extrapolateSteelProduction <- function(data) {
  # Extrapolate current by recnet for regions where data overlaps
  prod <- toolExtrapolate(data$current, ref=data$recent, 
                          extrapolate_method = 'ref')
  
  # calculate World total of regions where data is available until 1900
  world_ref <- getWorldSteelProductionTrend(prod, data$hist)
  
  # Extrapolate remaining regions by world reference
  prod <- toolExtrapolate(prod, ref=world_ref,
                          extrapolate_method='ref')
  
  return(prod)
}

getWorldSteelProductionTrend <- function(prod, prod_hist) {
  non_na_indices <- which(!is.na(prod[,1]))
  prod_non_na_regions <- prod[non_na_indices,]
  non_na_regions <- getItems(prod_non_na_regions, dim=1)
  
  mapping <- data.frame(from = non_na_regions, global = 'GLO')
  sum_non_na_regions <- toolAggregate(prod_non_na_regions, rel = mapping)
  
  # TODO: potentially the other way around? (Extrapolate hist to future with global recent sum)
  
  world_ref <- toolExtrapolate(x=sum_non_na_regions,
                               ref=prod_hist,
                               extrapolate_method='ref')
  
  return(world_ref)
}
  
  
  
  
  
  
  
  
