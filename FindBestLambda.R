###
# Cross-Validation for lambda using fully grown CART tree
# Interface exactly:
#   find_best_lambda(tree, lambdas, K=5, mode="regression")
###

source("Pruning.R")

############################################################
# prediction
############################################################

predict_tree_single <- function(node, X, i) {
  
  if (is.null(node$split_feature_j))
    return(node$prediction)
  
  j <- node$split_feature_j
  s <- node$split_value_i
  
  if (X[i, j] < s)
    predict_tree_single(node$left_child, X, i)
  else
    predict_tree_single(node$right_child, X, i)
}

predict_tree_vector <- function(tree, indices) {
  
  X <- tree$X
  
  sapply(indices, function(i)
    predict_tree_single(tree, X, i))
}

############################################################
# folds
############################################################

make_folds <- function(indices, K) {
  
  shuffled <- sample(indices)
  
  split(
    shuffled,
    cut(seq_along(shuffled), K, labels = FALSE)
  )
}

############################################################
# loss
############################################################

subset_loss <- function(tree, indices, mode) {
  
  y <- tree$y
  
  preds <- predict_tree_vector(tree, indices)
  
  if (mode == "regression")
    return(mean((y[indices] - preds)^2))
  
  if (mode == "classification")
    return(mean(preds != y[indices]))
}

############################################################
# MAIN
############################################################

find_best_lambda <- function(
    tree,
    lambdas,
    K = 5,
    mode = "regression"
) {
  
  if (is.null(tree$X) || is.null(tree$y))
    stop("Tree must contain tree$X and tree$y")
  
  y <- tree$y
  
  ##########################################################
  # pruning sequence once
  ##########################################################
  
  seq <- cost_complexity_sequence(tree, y)
  
  folds <- make_folds(tree$indices, K)
  
  cv_values <- rep(0, length(lambdas))
  
  ##########################################################
  # CV loop
  ##########################################################
  
  for (m in seq_along(folds)) {
    
    val_idx <- folds[[m]]
    
    for (l in seq_along(lambdas)) {
      
      pruned_tree <- select_tree_lambda(
        seq,
        lambdas[l],
        y
      )
      
      loss <- subset_loss(
        pruned_tree,
        val_idx,
        mode
      )
      
      cv_values[l] <- cv_values[l] + loss
    }
  }
  
  ##########################################################
  
  cv_values <- cv_values / K
  
  best_index <- which.min(cv_values)
  
  best_lambda <- lambdas[best_index]
  
  optimal_tree <- select_tree_lambda(
    seq,
    best_lambda,
    y
  )
  
  list(
    best_lambda = best_lambda,
    cv_values = cv_values,
    lambdas = lambdas,
    optimal_tree = optimal_tree,
    sequence = seq
  )
}