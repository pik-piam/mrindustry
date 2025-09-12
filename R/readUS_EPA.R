#' Read US_EPA Data into a magpie Object
#'
#' This function reads US EPA data on plastics EoL treatment in the US.
#'
#' @return magpie object of the US_EPA Data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' \dontrun{
#' a <- readSource(type = "US_EPA")
#' }
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' 
readUS_EPA <- function() {
  # ---------------------------------------------------------------------------
  # Read raw data from csv
  raw_df <- read.csv("PlasticEol.csv")
  
  # ---------------------------------------------------------------------------
  # Clean and pivot data
  df <- raw_df %>%
    select(-"Generation") %>%
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

