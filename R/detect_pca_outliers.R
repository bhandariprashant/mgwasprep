detect_pca_outliers <- function(pca_result, data, n_components = 3, md_threshold = 0.975, mad_multiplier = 3.5) {
  # Extract PC scores for the first n components
  scores <- as.data.frame(pca_result$x[, 1:n_components])

  # 1. Mahalanobis Distance
  md <- mahalonobis_outliers(scores, threshold = md_threshold)

  # 2. MAD-based detection for each component
  mad_outliers <- mad_based_outliers(scores, multiplier = mad_multiplier)

  # 3. Local Outlier Factor
  lof_scores <- lofactor(scores, k = 5)  # k=5 nearest neighbors
  lof_outliers <- lof_scores > quantile(lof_scores, 0.975)

  # Combine results
  outlier_status <- data.frame(
    row_id = 1:nrow(scores),
    mahalanobis_outlier = md,
    mad_outlier = mad_outliers,
    lof_outlier = lof_outliers,
    # Consider a point an outlier if flagged by at least 2 methods
    consensus_outlier = (md + mad_outliers + lof_outliers) >= 2
  )

  return(outlier_status)
}



