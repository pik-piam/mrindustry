#' Aggregate country data to the 21 REMIND regions (regionmapping_21_EU11)
#'
#' @description Aggregates ISO country data to the regions of `regionmapping_21_EU11.csv`, the
#' finest regional resolution REMIND is run with. The chemicals calculations use it as a fixed
#' internal resolution: nonlinear steps (ratios, residuals, shares) are evaluated at these regions,
#' independent of the regionmapping set in the madrat config, and without the artifacts that
#' evaluating them for single countries would produce.
#'
#' @param x magpie object at ISO country level, or the list returned by
#'   `calcOutput(..., aggregate = FALSE, supplementary = TRUE)`. In the latter case the weight and
#'   mixed_aggregation setting of that list are used, which reproduces the aggregation madrat would do.
#' @param weight optional weight (weighted mean, for intensive variables). NULL sums up.
#' @param mixedAggregation passed on as `mixed_aggregation` to [madrat::toolAggregate()]
#' @param zeroWeight passed on to [madrat::toolAggregate()]
#'
#' @return magpie object at the resolution of regionmapping_21_EU11
#'
#' @author Leonie Schweiger
toolAggregateToEU21 <- function(x, weight = NULL, mixedAggregation = FALSE, zeroWeight = "warn") {
  if (is.list(x) && !is.magpie(x)) {
    weight <- x$weight
    mixedAggregation <- isTRUE(x$mixed_aggregation)
    x <- x$x
  }
  map <- toolGetMapping("regionmapping_21_EU11.csv", type = "regional", where = "mappingfolder")
  if (!is.null(weight)) weight <- weight[getItems(x, dim = 1), , ]
  toolAggregate(x, rel = map, from = "CountryCode", to = "RegionCode", dim = 1, weight = weight,
                mixed_aggregation = mixedAggregation, zeroWeight = zeroWeight)
}
