mad_based_outliers <- function(scores, multiplier = 3.5) {
  # Check if point is outlier in any component
  outlier_by_comp <- sapply(scores, function(x) {
    med <- median(x)
    mad_val <- mad(x)
    lower <- med - multiplier * mad_val
    upper <- med + multiplier * mad_val
    x < lower | x > upper
  })
  # Consider point outlier if flagged in at least 2 components
  return(rowSums(outlier_by_comp) >= 2)
}
