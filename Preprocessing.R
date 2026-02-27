# This is a simple attribute rating algorithm for pre processing input data.
# It tries to rate each potential prediction attribute on a scale from 0 to 1.
# It does so by looking at the spearman correlation for numeric values or 
# average withing group variance for factors
# We expect: target_col = [numeric], data$ = [numeric, factor] 

attribute_rating_V1 <- function(data, target_col) {
  
  target <- data[[target_col]]
  prediction_attributes <- setdiff(names(data), target_col)
  scores <- numeric(length(prediction_attributes))
  names(scores) <- prediction_attributes
  
  for (i in seq_along(prediction_attributes)) {
    # extract column
    x <- data[[prediction_attributes[i]]]
    # filter for rows where prediction attribute and target exist
    valid <- !is.na(x) & !is.na(target)
 
    x <- x[valid]
    y <- target[valid]
    
    # remove if not enough data *values have been picked arbitrarily*
    if (length(x) < 5 || length(unique(x)) <= 1) {
      scores[i] <- 0
      next
    }
    
    # x = numeric, y = numeric
    if (is.numeric(x) && is.numeric(y)) {
      # rating the relationship between the x and y(target)
      r <- cor(x, y, method = "spearman")
      # storing the absolute values
      scores[i] <- abs(r)
      
      # x = factor, y = numeric
    } else if (is.factor(x) && is.numeric(y)) {
      # variability in y
      total_var <- var(y)
      
      # (calculating the variance for each "factor" 
      # * group size per variance) / total number 
      # => average withing group variance
      within_var <- sum(tapply(y, x, var) * tapply(y, x, length)) / length(y)
      
      # final score based on total variance per group
      scores[i] <- 1 - within_var / total_var
      
    } else {
      # unsupported type combos -> zero
      scores[i] <- 0
    }
  }
  
  # finding the maximal 
  max_s <- max(scores, na.rm = TRUE)
  # normalising values to get a max score of 1
  if (is.finite(max_s) && max_s > 0) scores <- scores / max_s
  # sorting values for final output
  ranked_attr <- sort(scores, decreasing = TRUE)
  names(ranked_attr)
}

