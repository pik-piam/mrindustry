#' Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @author Merlin Jo Hosak
#' @param x Magpie object
convertWorldSteelDigitised <- function(x, subtype="world_production") {
  # ---- Functions ----
  production_decade_convert <- function(x, withSCG=FALSE, modern=FALSE) {
    x <- x * 1e3  # convert from kt to t
    ignore <- c("Total Central America", "Total Industrial Cta_", "Total Mlddle East", "Total Western Europe without EU", "Total Western World","E_C_ Total", "Other Central America", "Other Middle East", "SubTotal", "Total Africa", "Total Asia", "Total Eastern Europe", "Total Latin America", "Total Middle East", "Total North America", "Total Oceania", "Total Western Europe")
    countries <- getItems(x,dim=1)
    
    map <- c("F_R_ of Germany"="DEU",
             "F_R_ Germany"="DEU",
             "F_ R_ Germany"="DEU",
             "German Democratic Republic" = "DDR",
             "German Dem_ Rep_" = "DDR",
             "Slovak  Republic"="SVK",
             "Byelorussia"="BLR",
             "Dem_ Rep_ Of Korea" = "PRK",
             "D_P_R_ Korea" = "PRK",
             "R_o_Korea"="KOR",
             "Luxemburg" = "LUX",
             "Phillipines" = "PHL",
             "Rhodesia" = "ZWE",
             "Yugoslavla" = "YUG",
             "F_R_ Yugoslavia" = "SCG",
             "Mainland China"="CHN",
             "Taiwan (R_O_C_)"="TWN",
             "Taiwan, China"="TWN",
             "former U_S_S_R_"="SUN",
             "Viet  Nam"="VNM",
             "Zaire"="COD",
             "U_S_S_R_"="SUN")
    
    getItems(x, dim=1) <- toolCountry2isocode(countries,ignoreCountries=ignore,mapping=map)
    # remove rows with NA in country_name column
    x <- x[!is.na(getItems(x, dim=1)), ]
    
    # remove other Asia and Africa (magpie maps them automatically to 
    # IAS and IAF, can not be ignored)
    x <- x[getItems(x, dim=1) != 'IAS',]
    x <- x[getItems(x, dim=1) != 'IAF',]
    
    
    # add two Germanies (West/East, FRG/DDR)
    x['DEU',] <- x['DEU',] + x['DDR',]
    x <- x[getItems(x, dim=1) != 'DDR', ]
    
    # split historical regions currently not possible due to wrong split year
    # y <- madrat::toolISOhistorical(x)
    
    # TODO: only interim solution -> e.g. Soviet Union should not just be 
    # mapped onto Russia, but split into its successor states, not possible 
    # for now because of historicla mapping. Also problematic because in 90s
    # potentially there is actually some data for Russia
    
    if (modern) {
      x <- toolCountryFill(x, verbosity=2)
      return(x)
    }
    
    soviet_union <- x['SUN',]
    czechoslobakia <- x['CSK',]
    yugoslavia <- x['YUG',]
    if (withSCG) {
      serbia_montenegro <- x['SCG',]
    }
    
    x <- x[getItems(x, dim=1) != 'SUN', ]
    x <- x[getItems(x, dim=1) != 'CSK', ]
    x <- x[getItems(x, dim=1) != 'YUG', ]
    x <- x[getItems(x, dim=1) != 'SCG', ]
    
    x <- toolCountryFill(x, verbosity=2)
    
    x['RUS',] <- soviet_union
    x['CZE',] <- czechoslobakia
    x['SRB',] <- yugoslavia
    
    if (withSCG) {
      x['SRB',] <- x['SRB',] + serbia_montenegro
    }
    
    # fill countries without data
    # z <- toolCountryFill(x, fill = NA, verbosity = 2)
    
    return(x)
  }
  
  # ---- list all available subtypes with functions doing all the work ----
  
  switchboard <- list(
    'world_production' = function(x) {
      x <- x * 1e6  # convert from Mt to t
      return(x)
    },
    
    'production_1969-1979' = function(x) {
      x <- production_decade_convert(x)
      return(x)
    },
    
    'production_1980-1989' = function(x) {
      x <- production_decade_convert(x)
      return(x)
    },
    
    'production_1990-1999' = function(x) {
      x <- production_decade_convert(x)
      return(x)
    },
    
    'production_2000-2009' = function(x) {
      x <- production_decade_convert(x)
      return(x)
    },
    
    NULL)
  
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]](x))
  }
  
  return(x)
}