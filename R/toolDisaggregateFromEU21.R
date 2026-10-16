#' Disaggregate data from the 21 REMIND regions (regionmapping_21_EU11) to ISO countries
#'
#' @description Counterpart of [toolAggregateToEU21()]. Extensive variables are split to the
#' countries of each region proportional to `weight`. Intensive variables (ratios, shares,
#' specific values) are disaggregated without weight, which copies the regional value to all
#' countries of the region; the calc function then has to return a matching aggregation weight.
#'
#' @param x magpie object at the resolution of regionmapping_21_EU11
#' @param weight optional ISO country weight used for splitting. NULL copies the regional value.
#' @param zeroWeight passed on to [madrat::toolAggregate()]
#'
#' @return magpie object at ISO country level
#'
#' @author Leonie Schweiger
toolDisaggregateFromEU21 <- function(x, weight = NULL, zeroWeight = "warn") {
  map <- toolGetMapping("regionmapping_21_EU11.csv", type = "regional", where = "mappingfolder")
  if (!is.null(weight)) weight <- weight[unique(map$CountryCode), , ]
  toolAggregate(x, rel = map, from = "RegionCode", to = "CountryCode", dim = 1, weight = weight,
                zeroWeight = zeroWeight)
}
