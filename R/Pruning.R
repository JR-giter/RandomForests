############################################################
# helper
############################################################

is_leaf <- function(node) {
  is.null(node$left_child) && is.null(node$right_child)
}

num_leaves <- function(node) {
  if (is_leaf(node)) return(1)
  num_leaves(node$left_child) + num_leaves(node$right_child)
}

clone_tree <- function(node) {
  
  if (is.null(node)) return(NULL)
  
  new <- new.env(parent = emptyenv())
  
  new$id <- node$id
  new$indices <- node$indices
  new$split_feature_j <- node$split_feature_j
  new$split_value_i <- node$split_value_i
  new$prediction <- node$prediction
  new$X <- node$X
  new$y <- node$y
  
  new$properties <- node$properties
  
  new$left_child <- clone_tree(node$left_child)
  new$right_child <- clone_tree(node$right_child)
  
  new
}

############################################################
# risk
############################################################

node_risk <- function(node, y) {
  
  idx <- node$indices
  
  if (length(idx)==0) return(0)
  
  pred <- mean(y[idx])
  
  sum((y[idx]-pred)^2)
}

subtree_risk <- function(node, y) {
  
  if (is_leaf(node))
    return(node_risk(node,y))
  
  subtree_risk(node$left_child,y) +
    subtree_risk(node$right_child,y)
}

############################################################
# weakest link g(t)
############################################################

compute_g <- function(node, y, result=list()) {
  
  if (is_leaf(node))
    return(result)
  
  Rt  <- node_risk(node,y)
  RTt <- subtree_risk(node,y)
  leaves <- num_leaves(node)
  
  g <- (Rt-RTt)/(leaves-1)
  
  result[[length(result)+1]] <- list(
    node=node,
    g=g
  )
  
  result <- compute_g(node$left_child,y,result)
  result <- compute_g(node$right_child,y,result)
  
  result
}

############################################################
# prune
############################################################

prune_node <- function(node) {
  
  node$left_child  <- NULL
  node$right_child <- NULL
  
  node$split_feature_j <- NULL
  node$split_value_i   <- NULL
}

############################################################
# sequence T0 ... TP
############################################################

cost_complexity_sequence <- function(tree,y) {
  
  trees  <- list()
  alphas <- c()
  
  current <- clone_tree(tree)
  
  trees[[1]] <- clone_tree(current)
  
  repeat {
    
    g_list <- compute_g(current,y)
    
    if (length(g_list)==0)
      break
    
    g_vals <- sapply(g_list,function(x)x$g)
    
    k <- which.min(g_vals)
    
    alpha <- g_vals[k]
    
    prune_node(g_list[[k]]$node)
    
    trees[[length(trees)+1]] <- clone_tree(current)
    alphas <- c(alphas,alpha)
    
    if (is_leaf(current))
      break
  }
  
  list(
    trees=trees,
    alphas=alphas
  )
}

############################################################
# select tree for lambda
############################################################

select_tree_lambda <- function(seq,lambda,y){
  
  best <- seq$trees[[1]]
  best_score <- Inf
  
  for(tree in seq$trees){
    
    R <- subtree_risk(tree,y)
    size <- num_leaves(tree)
    
    score <- R + lambda*size
    
    if(score<best_score){
      best_score <- score
      best <- tree
    }
  }
  
  best
}

############################################################
# prediction
############################################################

predict_single <- function(node,x){
  
  if(is.null(node$split_feature_j))
    return(node$prediction)
  
  j <- node$split_feature_j
  s <- node$split_value_i
  
  if(x[j] < s)
    predict_single(node$left_child,x)
  else
    predict_single(node$right_child,x)
}

predict_tree <- function(tree,X,indices){
  
  sapply(indices,function(i)
    predict_single(tree,X[i,])
  )
}

############################################################
# folds
############################################################

make_folds <- function(n,K){
  
  idx <- sample(1:n)
  
  split(idx,cut(seq_along(idx),K,labels=FALSE))
}

############################################################
# MAIN FUNCTION
#
# build_tree must be your original CART growing function
############################################################

############################################################
# CORRECT CART COST-COMPLEXITY PRUNING WITH K-FOLD CV
# works directly with your cart_tree object
############################################################

prune_tree <- function(tree, lambdas = NULL, K = 5, mode="regression")
{
  X <- tree$X
  y <- tree$y
  n <- length(y)
  
  ##########################################################
  # helper: rebuild tree using your greedy CART
  ##########################################################
  
  build_tree_from_indices <- function(indices)
  {
    Xsub <- X[indices,,drop=FALSE]
    ysub <- y[indices]
    
    new_tree <- greedy_cart_regression(Xsub, ysub)
    
    new_tree$X <- Xsub
    new_tree$y <- ysub
    
    new_tree
  }
  
  ##########################################################
  # prediction independent of stored X
  ##########################################################
  
  predict_single <- function(node, x)
  {
    if (is.null(node$split_feature_j))
      return(node$prediction)
    
    j <- node$split_feature_j
    s <- node$split_value_i
    
    if (x[j] < s)
      predict_single(node$left_child, x)
    else
      predict_single(node$right_child, x)
  }
  
  predict_tree <- function(tree, Xdata, indices)
  {
    sapply(indices, function(i)
      predict_single(tree, Xdata[i,]))
  }
  
  ##########################################################
  # make folds
  ##########################################################
  
  shuffled <- sample(1:n)
  folds <- split(shuffled, cut(seq_along(shuffled), K, labels=FALSE))
  
  ##########################################################
  # collect lambdas automatically if not supplied
  ##########################################################
  
  if (is.null(lambdas))
  {
    full_seq <- cost_complexity_sequence(tree, y)
    lambdas <- unique(full_seq$alphas)
  }
  
  cv_error <- rep(0, length(lambdas))
  
  ##########################################################
  # CROSS VALIDATION LOOP (correct according to CART theory)
  ##########################################################
  
  for (k in seq_along(folds))
  {
    val_idx <- folds[[k]]
    train_idx <- setdiff(1:n, val_idx)
    
    train_tree <- build_tree_from_indices(train_idx)
    
    seq <- cost_complexity_sequence(train_tree, train_tree$y)
    
    for (i in seq_along(lambdas))
    {
      lambda <- lambdas[i]
      
      pruned <- select_tree_lambda(seq, lambda, train_tree$y)
      
      preds <- predict_tree(pruned, X, val_idx)
      
      if (mode=="regression")
        err <- mean((y[val_idx] - preds)^2)
      else
        err <- mean(y[val_idx] != preds)
      
      cv_error[i] <- cv_error[i] + err
    }
  }
  
  cv_error <- cv_error / K
  
  ##########################################################
  # best lambda
  ##########################################################
  
  best_index <- which.min(cv_error)
  best_lambda <- lambdas[best_index]
  
  ##########################################################
  # prune full tree using best lambda
  ##########################################################
  
  full_seq <- cost_complexity_sequence(tree, y)
  
  optimal_tree <- select_tree_lambda(
    full_seq,
    best_lambda,
    y
  )
  
  ##########################################################
  # return same structure as your original function
  ##########################################################
  
  list(
    optimal_tree = optimal_tree,
    best_lambda = best_lambda,
    cv_values = cv_error,
    lambdas = lambdas,
    sequence = full_seq
  )
}
