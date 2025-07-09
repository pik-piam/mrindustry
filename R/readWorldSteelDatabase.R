#' Read Data from World Steel Association online Database,
#' downloaded to Excel files in the ./v1.0 directory. 
#' They should be updated yearly via the PIK's subscription to the database.
#' Most datasets are available between around 2002 and 2022 
#' on a yearly resolution.
#' @author Merlin Jo Hosak
#' @export
readWorldSteelDatabase <- function(subtype = 'production') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'production' = function() {
      x <- readxl::read_excel(path = paste0('./v1.0/',
                                            'P01_crude_2023-10-23.xlsx'),
                              range = 'A3:U99')
      x <- as.magpie(x, spatial="Country")
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
