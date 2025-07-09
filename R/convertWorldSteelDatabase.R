#' Convert data from WorldSteelAssociation Database
#' @author Merlin Jo Hosak
#' @param x Magpie object
convertWorldSteelDatabase <- function(x, subtype="production") {
  # ---- list all available subtypes with functions doing all the work ----
  
  switchboard <- list(
    'production' = function(x) {
      # convert from kt to t
      x <- x * 1e3
      
      # replace country names with ISO codes
      
      countries <- getItems(x, dim=1)
      countries <- gsub('_', '.', countries)  # replace _ with . for isocode conversion
      getItems(x, dim=1) <- toolCountry2isocode(countries,ignoreCountries=c('Others'))
      
      # Add soviet union countries that are not part of the dataset yet and fill
      # them with 0, as they are needed to split soviet union data into current
      # countries.
      
      missing_countries <- new.magpie(
        cells_and_regions = c('KGZ', 'TJK', 'TKM'),
        years = getItems(x, dim=2),
        names = "value",
        fill = 0,
        sets = names(dimnames(x))
      )
      x <- mbind(x, missing_countries)
      
      # Add historical mapping for Yugoslavia with last year being 2005
      # instead of 1991 as there is some aggregated data in this dataset
      # for the years 2002-2005 for Yugoslavia.
      
      historical_mapping <- list(fromISO = c('YUG', 'YUG', 'YUG', 'YUG', 'YUG', 'YUG'),
                                 toISO = c('SRB', 'MNE', 'SVN', 'HRV', 'MKD', 'BIH'),
                                 lastYear = c('y2005', 'y2005', 'y2005', 'y2005', 'y2005', 'y2005'))
      
      historical_mapping <- as.data.frame(historical_mapping)
      
      x <- toolISOhistorical(x, overwrite=TRUE, mapping=historical_mapping)
      x <- toolISOhistorical(x)
      
      # remove rows with NA in country_name column
      x <- x[!is.na(getItems(x, dim=1)), ]
      
      x <- toolCountryFill(x, verbosity=2)
      
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