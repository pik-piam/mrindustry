toolInterpolate2D <- function(x, method='linear') {
  x <- getSteelTradeData(subtype)$recent
  df <- mtab(x)
  df <- as.data.frame(df)
  # make region column row index
  rownames(df) <- df[[1]]
  # delete first column
  df <- df[-1]
  
  # transpose ddf
  tdf <- t(df)
  tdfy <- zoo::na.approx(tdf, method='linear',rule=1)
  dfy <- t(dfy)
  y <- as.magpie(dfy)
  
}