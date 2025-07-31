#' Get GDP from 1900-2150
#' @description
#' Calc GDP (PPP) from 1900-2150 yearly for the SIMSON format on a country 
#' level. Can be aggregated to regions via calcOutput aggregate parameter.
#' Uses \link[=readOECD_GDP]{OECD_GDP} for historical GDP data as well as
#' \link[=calcGDP]{calcGDP} from mrdrivers for current and future GDP data
#' according to a specific scenario (see \code{vignette("scenarios")} 
#' for more information). Population data from 
#' \link[=calcPopulation200]{calcPopulation200} is used to convert 
#' GDP per capita to total GDP.
#' GDP is given in 2005 USD (PPP). It's extrapolated to the past with historic
#' GDP datasets that use a different base year, which however does not matter
#' as only the relative values are used 
#' (see \link[=toolInterpolate]{toolInterpolate}).
#' @author Merlin Jo Hosak
#' @param scenario Scenario to use for future GDP data (default: SSP2).
#' @param per_capita If TRUE, GDP is returned as per capita (default: FALSE).
#' @return List with Magpie object of GDP and metadata in calcOutput format.
#' @export
calcGDP1900To2150 <- function(scenario='SSP2', per_capita=FALSE) {
  # load data
  data <- getGDP1900To2150Data(scenario=scenario)
  
  # interpolate
  data <- interpolateGDP1900To2150(data)
  
  # convert per capita to total
  data$hist <- data$pop[,1:117] * data$hist_pc
  
  # extrapolate
  data <- extrapolateGDP1900To2150(data)
  
  gdp <- data$final
  
  # finalize for calcOutput
  
  unit <- '2005 USD$PPP'
  description='GDP from 1900-2150 yearly for the SIMSON format'
  weight<-NULL
  
  # convert to per capita if requested
  if (per_capita) {
    gdp <- gdp / data$pop
    unit <- '2005 USD$PPP per capita'
    description='GDP per capita from 1900-2150 yearly for the SIMSON format'
    weight <- data$pop
  } 
  
  # check if there are any NA left in gdp
  if (any(is.na(gdp))) {
    warning("There are still NA values in the GDP data.")
  }
  
  result <- list(x = gdp, 
                 weight = weight,  # TODO adapt weight for per capita data
                 unit=unit,
                 description=description)
  
  
  return(result)
}

getGDP1900To2150Data <- function(scenario){
  # load data
  pop <- calcOutput('Population1900To2150', aggregate=F)
  gdp_pc_hist <- readSource('OECD_GDP', subtype='gdppc',convert=T)
  gdp_recent <- calcOutput('GDP', scenario=scenario, aggregate=F)
  
  # convert format
  gdp_recent <- gdp_recent * 1e6  # convert to million USD  
  getItems(gdp_recent,dim=3) <- 'value'
  
  return(list(pop=pop, 
              hist_pc=gdp_pc_hist,
              recent=gdp_recent))
}

interpolateGDP1900To2150 <- function(data) {
  # interpolate
  data$recent <- time_interpolate(data$recent, 1965:2150)
  
  data$hist_pc <- toolInterpolate(data$hist_pc, method = 'linear')
  data$hist_pc <- data$hist_pc[,1900:2016]  # data before 1900 irrelevant
  
  return(data)
}

extrapolateGDP1900To2150 <- function(data) {
  # Extrapolate data with OECD data as reference where data is available
  data$final <- toolExtrapolate(data$recent, extrapolate_method = 'ref', ref=data$hist)
  
  # Extrapolate GDP data by global total for regions without OECD data
  
  ## get GDP of regions that have data up to 1900
  regions <- getRegions(data$final)
  regions_not_na <- regions[!is.na(data$final[,1])]
  gdp_from_1900 <- data$final[regions_not_na,]
  
  ## sum over these regions
  mapping <- data.frame(from = regions_not_na, global = 'GLO')
  sum_gdp_from_1900 <- toolAggregate(gdp_from_1900, rel = mapping)
  
  # Extrapolate missing regions with the global average
  data$final <- toolExtrapolate(data$final, extrapolate_method = 'ref', ref = sum_gdp_from_1900)
  
  return(data)
}






