#' Read Data from World Steel Association 1978-2022 yearbooks digitized to Excel sheets
#' E.g. from 1982: https://worldsteel.org/wp-content/uploads/Steel-Statistical-Yearbook-1982.pdf
#' @author Merlin Jo Hosak
readWorldSteelDigitised <- function(subtype = 'world_production') {
  # ---- Functions ----
  decade_read <- function(name) {
    x <- readxl::read_excel(path = name)
    # delete rows with NA in country_name column
    x <- x[!is.na(x$country_name), ]
    x <- as.magpie(x, spatial='country_name')
    return(x)
  }
  
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'world_production' = function() {
      x <- readxl::read_excel(path = paste0('./v1.0/production/',
                                               'world_production_1900-1979.xlsx'),
                                 range = 'A4:B84')
      x <- as.magpie(x)
      return(x)
      },
    'production_1969-1979' = function() {
      x <- decade_read(paste0('./v1.0/production/',
                              'production_1969-1979.xlsx'))
      return(x)
    },
    'production_1980-1989' = function() {
      x <- decade_read(paste0('./v1.0/production/',
                              'production_1980-1989.xlsx'))
      return(x)
    },
    'production_1990-1999' = function() {
      x <- decade_read(paste0('./v1.0/production/',
                              'production_1990-1999.xlsx'))
      return(x)
    },
    'production_2000-2009' = function() {
      x <- decade_read(paste0('./v1.0/production/',
                              'production_2000-2009.xlsx'))
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
