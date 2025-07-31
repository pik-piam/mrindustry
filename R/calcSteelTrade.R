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
calcSteelTrade <- function(subtype = 'imports') {
  if (subtype %in% c('indirect_imports', 'indirect_exports')) {
    return(calcSteelIndirectTrade(subtype))
  }
  
  data <- getSteelTradeData(subtype)
  
  # Interpolate
  data$digitised <- toolInterpolate(data$digitised)
  data$database <- toolInterpolate(data$database)
  
  data$digitised[data$digitised<1] = NA # if values are too small, they are not fit for extrapolation by reference (potentially creating infinite/unrealistic values)
  
  # Extrapolate
  result <- toolExtrapolate(data$database, ref=data$digitised, 
                            extrapolate_method = 'ref')
  result <- toolExtrapolate(result, ref=data$production, 
                            extrapolate_method = 'ref')
  
  # Finalize for calcOutput
  result[is.na(result)] <- 0 ## fill remaining NA with zero
  
  result <- list(x = result, 
                 weight = NULL,
                 unit='Tonnes',
                 description=paste0('Steel trade:', subtype, 
                                    'from 1900-2021 yearly for the SIMSON format.'))
  
  return(result)
}

calcSteelIndirectTrade <- function(subtype) {
  data <- getSteelTradeData(subtype, by_category_2013=TRUE)
  
  # Interpolate
  data$database <- toolInterpolate(data$database)
  
  # Extrapolate
  result <- toolExtrapolate(data$database, ref=data$production, 
                            extrapolate_method = 'ref')
  
  # Fill gaps
  result[is.na(result)] <- 0 ## fill remaining NA with zero
  
  # Multiply by shares
  
  shares <- data$digitised
  
  intersecting_countries <- intersect(getItems(result, 1), getItems(shares, 1))
  result <- result[intersecting_countries, ]
  shares <- shares[intersecting_countries, ]
  
  result <- result * shares
  
  result <- toolCountryFill(result, verbosity=2, fill=0) # Fill missing countries with zeroes
  
  result[is.na(result)] <- 0 ## fill remaining NA with zero
  
  
  # Finalize for calcOutput
  result <- list(x = result, 
                 weight = NULL,
                 unit='Tonnes',
                 description=paste0('Steel trade:', subtype, 
                                    'from 1900-2021 yearly for the SIMSON format.'))
  
  return(result)
  
}

getSteelTradeData <- function(subtype='imports', by_category_2013=FALSE) {
  # load data
  production <- calcOutput('SteelProduction', aggregate=FALSE)
  database <- readSource('WorldSteelDatabase', subtype=subtype)
  
  if (by_category_2013) {
    subtype = paste0(subtype, '_by_category_2013')
  }
  
  digitised <- readSource('WorldSteelDigitised', subtype=subtype, convert=!by_category_2013)
  
  return(list(production=production,
              database=database,
              digitised=digitised))
}






