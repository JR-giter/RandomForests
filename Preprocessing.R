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


 attribute_rating_V2 <- function(data, target_col) {
   
 }
 
 
# =============================================================
# "Best" rating to compare to
# =============================================================

library(FSelectorRcpp)

attribute_rating_V3 <- function(data, target_col) {
  formula <- as.formula(paste(target_col, "~ ."))
  scores <- information_gain(formula, data)
  scores$attributes[order(scores$importance, decreasing = TRUE)]
}

compare_rankings <- function(ranking_func1, ranking_func2, data, target_col, mode = c("all", "numeric")) {
  
  mode <- match.arg(mode)
  
  # Compute rankings using the provided functions
  ranking1 <- ranking_func1(data, target_col)
  ranking2 <- ranking_func2(data, target_col)
  
  # Optionally filter only numeric attributes
  if (mode == "numeric") {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    ranking1 <- ranking1[ranking1 %in% numeric_cols]
    ranking2 <- ranking2[ranking2 %in% numeric_cols]
  }
  
  # Keep only common attributes
  common <- intersect(ranking1, ranking2)
  if (length(common) < 2) return(NA)  # Not enough to compare
  
  # Convert attribute names to numeric ranks
  ranks1 <- match(common, ranking1)
  ranks2 <- match(common, ranking2)
  
  # Spearman correlation
  spearman_corr <- cor(ranks1, ranks2, method = "spearman")
  
  # Convert to 0–100% agreement
  agreement_percent <- (spearman_corr + 1) / 2 * 100
  agreement_percent
}

compare_rankings(attribute_rating_V1, attribute_rating_V3, ames, "Sale_Price")
