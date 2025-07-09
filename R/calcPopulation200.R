#' Get population from 1900-2100 
#' 
#' @description 
#' Calc population from 1900-2100 yearly for the SIMSON format on a country 
#' level. Can be aggregated to regions via calcOutput aggregate parameter.
#' Uses \link[=readGapminder]{Gapminder} and \link[=readUN_PopDiv]{UN_PopDiv} 
#' datasets for historical population data as well as calcPopulation from 
#' mrdrivers for current and future population data according to a specific 
#' scenario (see \code{vignette("scenarios")} for more information).
#' 
#' @author Merlin Jo Hosak
#' @param scenario Scenario to use for future population data (default: SSP2).
#' @return List with Magpie object of population and metadata in calcOutput 
#' format.
#' @export
calcPopulation200 <- function(scenario='SSP2') {
  pop <- getPopulation200Data(scenario=scenario)
  
  # get yearly resolution for future
  pop$future <- time_interpolate(pop$current,2030:2100)
  pop$current <- mbind(pop$current[,1960:2029],pop$future)

  # extrapolate with Gapminder dataset as reference data for countries where such data exists
  pop$final <- toolExtrapolate(x=pop$current, ref=pop$hist, extrapolate_method = 'ref')
  
  # extrapolate with world average as reference data for other countries
  pop$final <- toolExtrapolate(x=pop$final, ref=pop$world_hist, extrapolate_method = 'ref')
  
  
  # check if there are any NA left in pop
  if (any(is.na(pop$final))) {
    warning("There are still NA values in the population data after extrapolation.")
  }
  
  result <- list(x = pop$final, 
                 weight = NULL,
                 unit='inhabitants',
                 description='Population from 1900-2100 yearly for the SIMSON format')
  
  
  return(result)
}


getPopulation200Data <- function(scenario) {
  # Load datasets, convert to inhabitants, get one year resolution 
  # via linear interpolation
  
  # The Gapminder dataset reaches from 1800 to 2100, but lacks a few small 
  # regions and lacks scenario information. Hence it is only used for 
  # extrapolation of scenario data in the 20th century.
  pop_hist <- readSource('Gapminder', subtype='population')
  pop_hist <- pop_hist[,1900:2000]
  
  # The UN_PopDiv dataset reaches from 1900 to 2150, in 5 year steps and is 
  # therefore interpolated to 1 year resolution. It is used to extrapolate 
  # 20th century data for the remaining regions.
  pop_world_hist <- readSource('UN_PopDiv', subtype='pop', subset='1900-2150', convert=F)
  pop_world_hist <- time_interpolate(pop_world_hist,1900:2100)
  
  # The mrdrivers calcPopulation function provides population data from 1960 on
  pop_current <- calcOutput('Population', scenario=scenario, aggregate=F)
  getItems(pop_current, dim=3) <- 'value'
  pop_current <- pop_current * 1e6 # convert from millions to inhabitants
  
  return(list(hist=pop_hist, 
              world_hist=pop_world_hist, 
              current=pop_current))
}








