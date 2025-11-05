#' Calculate Energy Balances Output to Industry
#' Additional corrections are applied to the IEA data in [`mrindustry::tool_fix_IEA_data_for_Industry_subsectors`].
#'
#' @author Michaja Pehl, Falk Benke
calcEnergyBalancesOutputToIndustry <- function() {

  ieamatch <- toolGetMapping(type = "sectoral",
                             name = "structuremappingIO_outputs_Industry_subsectors.csv",
                             where = "mrindustry",
                             returnPathOnly = FALSE)

  target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")

  ieamatch <- ieamatch %>%
    select(tidyselect::all_of(c("iea_product", "iea_flows", "Weight", target))) %>%
    stats::na.omit() %>%
    tidyr::unite("target", tidyselect::all_of(target), sep = ".", remove = FALSE) %>%
    tidyr::unite("product.flow", c("iea_product", "iea_flows"), sep = ".")

  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  # apply corrections to IEA data to cope with fragmentary time series
  namesBefore <- getNames(data)
  data <- tool_fix_IEA_data_for_Industry_subsectors(data)

  # warn if product flows not present in the mapping have been added to the data
  newItems <- setdiff(getNames(data), namesBefore)
  productFlows <- unique(pull(ieamatch, "product.flow"))
  newProductFlows <- setdiff(newItems, productFlows)

  # FIXME: investigate, as these product flows mean potential losses after mapping
  if (!rlang::is_empty(newProductFlows)) {
    message("Product/flow combinations not present in mapping added by ",
            "fix_IEA_data_for_Industry_subsectors():\n",
            paste(newProductFlows, collapse = "\n")
    )
  }

  reminditems <-  do.call(
    mbind,
    lapply(unique(ieamatch$target),
           function(item) {
             product_flow <- ieamatch %>%
               filter(item == .data$target) %>%
               pull("product.flow")

             weights <- ieamatch %>%
               filter(item == .data$target) %>%
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
