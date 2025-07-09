#' Read Data from World Steel Association 1978-2022 yearbooks digitized to Excel sheets
#' E.g. from 1982: https://worldsteel.org/wp-content/uploads/Steel-Statistical-Yearbook-1982.pdf
#' @author Merlin Jo Hosak
#' @export
readWorldSteelDigitised <- function(subtype = 'world_production') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'world_production' = function() {
      x <- readxl::read_excel(path = paste0('./v1.0/production/',
                                               'world_production_1900-1979.xlsx'),
                                 range = 'A4:B84')
      x <- as.magpie(x)
      
      # convert from Mt to t
      # conversion needs to happen here, as return value of conversion expects values for all countries
      x <- x * 1e6
      
      getItems(x,dim=3) <- 'value'
      
      return(x)
      },
    
    'production_1969-2009' = function() {
      # TODO: Make this a more generic generic for other WSA data as well.
      
      x70s <- toolDecadeRead(paste0('./v1.0/production/',
                                    'production_1969-1979.xlsx'))
      x80s <- toolDecadeRead(paste0('./v1.0/production/',
                                    'production_1980-1989.xlsx'))
      x90s <- toolDecadeRead(paste0('./v1.0/production/',
                                    'production_1990-1999.xlsx'))
      x00s <- toolDecadeRead(paste0('./v1.0/production/',
                                    'production_2000-2009.xlsx'))
      allCountries <- union(getItems(x70s, dim=1),
                            union(getItems(x80s, dim=1),
                                  union(getItems(x90s, dim=1),
                                        getItems(x00s, dim=1))))
      allYears <- union(getItems(x70s, dim=2),
                        union(getItems(x80s, dim=2),
                              union(getItems(x90s, dim=2),
                                    getItems(x00s, dim=2))))
      
      x <- new.magpie(
        cells_and_regions = allCountries,
        years = allYears,
        names = "value",
        fill = NA,
        sets = names(dimnames(x70s))
      )
      
      # fill in the data
      
      x[getItems(x70s, dim=1), getItems(x70s, dim=2)] <- x70s
      x[getItems(x80s, dim=1), getItems(x80s, dim=2)] <- x80s
      x[getItems(x90s, dim=1), getItems(x90s, dim=2)] <- x90s
      x[getItems(x00s, dim=1), getItems(x00s, dim=2)] <- x00s
      
      return(x)
    },
    
    NULL)
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}

# ---- Functions ----

toolDecadeRead <- function(name) {
  # TODO: Make this a more generic generic for other WSA data as well.
  x <- readxl::read_excel(path = name)
  # delete rows with NA in country_name column
  x <- x[!is.na(x$country_name), ]
  x <- as.magpie(x, spatial='country_name')
  
  
  
  # Total (aggregated) data is ignored as well as 'Other' Regions like 
  ignore <- c("Total Central America", "Total Industrial Cta.", "Total Mlddle East", "Total Western Europe without EU", "Total Western World","E.C. Total", "Other Central America", "Other Middle East", "SubTotal", "Total Africa", "Total Asia", "Total Eastern Europe", "Total Latin America", "Total Middle East", "Total North America", "Total Oceania", "Total Western Europe")
  
  countries <- getItems(x,dim=1)
  countries <- gsub('_', '.', countries)  # replace underscores with dots as magclass sometimes does the opposite
  getItems(x, dim=1) <- toolCountry2isocode(countries,ignoreCountries=ignore)
  # remove rows with NA in country_name column
  x <- x[!is.na(getItems(x, dim=1)), ]
  
  
  # remove other Asia and Africa (magpie maps them automatically to 
  # IAS and IAF, can now ignored as country level data is available?)
  x <- x[getItems(x, dim=1) != 'IAS',]
  x <- x[getItems(x, dim=1) != 'IAF',]
  
  return(x)
}

