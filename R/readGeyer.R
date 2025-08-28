#' Read Geyer et al. SI Global plastic production 1950-2015
#'
#' @param subtype Character string specifying the dataset and scope.
#'        For now only one subtype, but SI contains more data that we might to read in in the future:
#'        - "Prod_1950-2015"
#'
#' @return magpie object of the Geyer et al. Plastic data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' \dontrun{
#' a <- readSource(type = "Geyer", subtype = "Prod_1950-2015")
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter
#' @importFrom magclass as.magpie getComment<-
#' 
readGeyer <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map key to Excel file parameters
  params <- switch(subtype,
                   "Prod_1950-2015" = list(
                     file   = "Geyer.xlsx",
                     sheet  = "Table S1",
                     range  = "A1:B67"
                   ),
                   stop("Invalid subtype: ", subtype)
  )
  
  # ---------------------------------------------------------------------------
  # Read raw data from Excel
  raw_df <- read_excel(
    path  = "C:/Users/leoniesc/madrat/sources/Geyer/Geyer.xlsx", #params$file,
    sheet = params$sheet,
    range = params$range,
    skip  = 1
  )
  
  # ---------------------------------------------------------------------------
  # Select and filter columns based on subtype
  df <- switch(
    subtype,
    # Plastic use or waste by region
    "Prod_1950-2015" = raw_df %>%
      select(`Year`, `Global Resin Production (Mt)`),
    stop("Unsupported subtype: ", subtype)
  )
  
  # ---------------------------------------------------------------------------
  # Convert to magpie object
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(df, temporal = 1)
  getComment(magpie_data) <- subtype
  
  return(magpie_data)
}

