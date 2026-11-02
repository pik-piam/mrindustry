#' Filters chemical route flows from calcAllChemicalRoutes_2005to2020 for the year 2020
#' can be removed if calcAllChemicalRoutes_2005to2020 is read into REMIND
#' 
#' @author Qianzhi Zhang
#'
#' @param CCS boolean parameter whether CCS technologies are considered as such in 2020 or assumed to be technologies without CCS
#' 
calcAllChemicalRoutes_2020 <- function(CCS=FALSE) {
  
  AllChemicalRoutes_2020 <- calcOutput("AllChemicalRoutes_2005to2020", CCS = CCS, warnNA = FALSE, aggregate = FALSE)[, "y2020", ]

  return(list(
    x = AllChemicalRoutes_2020,
    weight = NULL,
    unit = "Gt Chemical Routes",
    description = "Chemical route flows filtered for the year 2020"
  ))
}
