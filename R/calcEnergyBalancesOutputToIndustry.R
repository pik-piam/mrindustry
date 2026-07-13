#' Calculate Energy Balances Output to Industry
#'
#' @author Michaja Pehl, Falk Benke
#' @importFrom mrcommonsenergy toolFixIeaDataForIndustrySubsectors
#'
calcEnergyBalancesOutputToIndustry <- function() {

  ieamatch <- toolGetMapping(type = "sectoral",
                             name = "structuremappingIO_outputs.csv",
                             where = "mrcommonsenergy",
                             returnPathOnly = FALSE)

  target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")

  ieamatch <- ieamatch %>%
    select(tidyselect::all_of(c("iea_product", "iea_flows", "Weight", target))) %>%
    stats::na.omit() %>%
    tidyr::unite("target", tidyselect::all_of(target), sep = ".", remove = FALSE) %>%
    tidyr::unite("product.flow", c("iea_product", "iea_flows"), sep = ".")

  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  reminditems <-  do.call(
    mbind,
    lapply(unique(ieamatch$target),
           function(item) {
             product_flow <- ieamatch %>%
               filter(item == .data$target,
                      .data$product.flow %in% getNames(data)) %>%
               pull("product.flow")

             weights <- ieamatch %>%
               filter(item == .data$target,
                      .data$product.flow %in% getNames(data)) %>%
               pull("Weight") %>%
               as.numeric()

             tmp <- dimSums(data[, , product_flow]
                            * setNames(as.magpie(weights), product_flow),
                            dim = 3, na.rm = TRUE)
             getNames(tmp) <- item

             return(tmp)
           })
  )


  return(list(x = reminditems,
              weight = NULL,
              unit = "EJ",
              description = "IEA Industry Subsector Output Data based on IEA World Energy Balances")
  )

}
