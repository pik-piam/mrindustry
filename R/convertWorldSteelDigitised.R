#' Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @author Merlin Jo Hosak
#' @param x Magpie object
convertWorldSteelDigitised <- function(x, subtype="production_1969-2009") {
  # ---- list all available subtypes with functions doing all the work ----
  
  switchboard <- list(
    'production_1969-2009' = function(x) {
      x <- x * 1e3  # convert from kt to t
      
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
      
      # TODO rewrite?:
      #
      # Add two Germanies (West/East, FRG/DDR). Another option would be to label 
      # the West German data before 1990 as 'BRG' instead of 'DEU' and then
      # toolISOhistorical would add it automatically, but this would add way more 
      # lines of code then necessary. This would also entail mapping GDR data to
      # 'GDR' instead of 'DDR' which is its (former) official ISO code.
      # The other option is to change the mapping 'ISOhistorical.csv'
      # for that function to automatically perform this operation, but this will
      # likely create problems with other files that have DEU (West Germany) data 
      # but no DDR data. Lastly, one could add an additional mapping in the 
      # function call like this:
      # y<-toolISOhistorical(x,mapping=list(c('DDR','DEU','y1990'),c('DEU','DEU','y1990')),overwrite=T)
      # but when tried it does not work because the function still expects GDR/BRG
      # data. Hence the easiest option here:
      
      
      
    
      
      # countries are split by share in the first year where new countries were 
      # formed, thereby getting rid of Yugoslavia (YUG), Czechoslovakia (CSK),
      # Soviet Union (SUN) and F.R. Yugoslavia / Serbia & Montenegro (SCG)
      # warnings suppressed as in some of these countries (e.g. Estonia 1992 no 
      # data was available and hence there it's replaced with 0)
      y <- suppressWarnings(toolISOhistorical(x, overwrite=TRUE))
      
      # Fill missing countries with NA values, will be changed in calc file.
      # Verbosity is 2 so that no warning shows up about these added countries.
      z <- toolCountryFill(y, verbosity=2) 
      
      return(z)
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