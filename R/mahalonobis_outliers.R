mahalonobis_outliers <- function(scores, threshold = 0.975) {
  center <- colMeans(scores)
  cov_mat <- cov(scores)
  md <- mahalanobis(scores, center, cov_mat)
  # Compare to chi-square distribution
  cutoff <- qchisq(threshold, df = ncol(scores))
  return(md > cutoff)
}
