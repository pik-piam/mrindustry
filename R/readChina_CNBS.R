#' Read China_CNBS Data into a magpie Object
#'
#' This function reads China CNBS data on plastics EoL treatment in China.
#'
#' @return magpie object of the China_CNBS Data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' \dontrun{
#' a <- readSource(type = "China_CNBS")
#' }
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' 
readChina_CNBS <- function() {
  # ---------------------------------------------------------------------------
  # Read raw data from csv
  raw_df <- read.csv("PlasticEol.csv")
  
  # ---------------------------------------------------------------------------
  # Clean and pivot data
  df <- raw_df %>%
    select(-"Source") %>%
    pivot_longer(
      cols = -"Year",
      names_to = "Treatment",
      values_to = "Value"
    )
  
  # ---------------------------------------------------------------------------
  # Convert to magpie object and clean missing values
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(df, temporal = 1)
  
  magpie_data[is.na(magpie_data)] <- 0
  
  return(magpie_data)
}

