###
# Cross-Validation for Cost-Complexity Pruning (Bemerkung 6.21)
###

source("Pruning.R")

############################################################
# Prediction
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


predict_tree_vector <- function(tree, X, indices) {
  
  sapply(indices, function(i)
    predict_tree_single(tree, X, i))
}

############################################################
# Risk restricted to subset
############################################################

subset_risk_regression <- function(tree, X, y, indices) {
  
  preds <- predict_tree_vector(tree, X, indices)
  
  mean((y[indices] - preds)^2)
}


subset_risk_classification <- function(tree, X, y, indices) {
  
  preds <- predict_tree_vector(tree, X, indices)
  
  mean(preds != y[indices])
}

############################################################
# Create folds
############################################################

make_folds <- function(n, K) {
  
  shuffled <- sample(1:n)
  
  split(shuffled, cut(seq_along(shuffled), K, labels = FALSE))
}

############################################################
# MAIN FUNCTION
############################################################

find_best_lambda <- function(
    X,
    y,
    lambdas,
    K = 5,
    type = "regression"
) {
  
  n <- length(y)
  
  folds <- make_folds(n, K)
  
  cv_values <- rep(0, length(lambdas))
  
  ##########################################################
  # Cross-validation loop
  ##########################################################
  
  for (m in seq_along(folds)) {
    
    val_idx <- folds[[m]]
    
    train_idx <- setdiff(1:n, val_idx)
    
    ######################################################
    # grow full tree using YOUR CART
    ######################################################
    
    if (type == "regression") {
      
      full_tree <- greedy_cart_regression(
        X[train_idx, , drop = FALSE],
        y[train_idx]
      )
      
    } else {
      
      full_tree <- greedy_cart_classification(
        X[train_idx, , drop = FALSE],
        y[train_idx]
      )
      
    }
    
    ######################################################
    # pruning sequence
    ######################################################
    
    seq <- cost_complexity_sequence(full_tree, y[train_idx])
    
    ######################################################
    # evaluate lambdas
    ######################################################
    
    for (l in seq_along(lambdas)) {
      
      lambda <- lambdas[l]
      
      pruned_tree <- select_tree_lambda(
        seq,
        lambda,
        y[train_idx]
      )
      
      if (type == "regression") {
        
        loss <- subset_risk_regression(
          pruned_tree,
          X,
          y,
          val_idx
        )
        
      } else {
        
        loss <- subset_risk_classification(
          pruned_tree,
          X,
          y,
          val_idx
        )
      }
      
      cv_values[l] <- cv_values[l] + loss
    }
  }
  
  ##########################################################
  # average CV error
  ##########################################################
  
  cv_values <- cv_values / K
  
  best_index <- which.min(cv_values)
  
  best_lambda <- lambdas[best_index]
  
  ##########################################################
  # final tree on full dataset
  ##########################################################
  
  if (type == "regression") {
    
    full_tree <- greedy_cart_regression(X, y)
    
  } else {
    
    full_tree <- greedy_cart_classification(X, y)
  }
  
  final_seq <- cost_complexity_sequence(full_tree, y)
  
  optimal_tree <- select_tree_lambda(
    final_seq,
    best_lambda,
    y
  )
  
  ##########################################################
  
  list(
    best_lambda = best_lambda,
    cv_values = cv_values,
    lambdas = lambdas,
    optimal_tree = optimal_tree
  )
}
