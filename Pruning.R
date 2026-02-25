###
#
# Pruning Implementation
# Input: Tree from Greey CART
# Output: Firstly Sequence of Pruned Trees, then optimal Tree depending on lambda
#
##
is_leaf <- function(node) {
  is.null(node$left_child) && is.null(node$right_child)
}

num_leaves <- function(node) {
  if (is_leaf(node)) return(1)
  num_leaves(node$left_child) + num_leaves(node$right_child)
}

clone_tree <- function(node) {
  new <- new.env(parent = emptyenv())
  
  new$id <- node$id
  new$indices <- node$indices
  new$split_feature_j <- node$split_feature_j
  new$split_value_i <- node$split_value_i
  new$prediction <- node$prediction
  
  if (!is.null(node$left_child))
    new$left_child <- clone_tree(node$left_child)
  else
    new$left_child <- NULL
  
  if (!is.null(node$right_child))
    new$right_child <- clone_tree(node$right_child)
  else
    new$right_child <- NULL
  
  new
}
# Risk Funktionen
node_risk <- function(node, y) {
  
  idx <- node$indices
  
  if (length(idx) == 0)
    return(0)
  
  pred <- mean(y[idx])
  
  sum((y[idx] - pred)^2)
}

subtree_risk <- function(node, y) {
  
  if (is.null(node$left_child))
    return(node_risk(node, y))
  
  subtree_risk(node$left_child, y) +
    subtree_risk(node$right_child, y)
}

# Compute G
compute_g <- function(node, y, result=list()) {
  
  if (is_leaf(node))
    return(result)
  
  Rt <- node_risk(node, y)
  RTt <- subtree_risk(node, y)
  leaves <- num_leaves(node)
  
  g <- (Rt - RTt) / (leaves - 1)
  
  result[[length(result)+1]] <- list(
    node=node,
    g=g
  )
  
  result <- compute_g(node$left_child, y, result)
  result <- compute_g(node$right_child, y, result)
  
  result
}
###
# Pruning
###
prune_node <- function(node) {
  
  if (is.null(node$id) || length(node$id)==0)
    node$id <- new_node_id()
  
  node$left_child <- NULL
  node$right_child <- NULL
  
  node$split_feature_j <- NULL
  node$split_value_i <- NULL
}

cost_complexity_sequence <- function(tree, y) {
  
  seq <- list()
  alphas <- c()
  
  current <- clone_tree(tree)
  seq[[1]] <- clone_tree(current)
  
  repeat {
    
    g_list <- compute_g(current, y)
    
    if (length(g_list) == 0)
      break
    
    g_values <- sapply(g_list, function(x) x$g)
    min_index <- which.min(g_values)
    
    alpha <- g_values[min_index]
    node_to_prune <- g_list[[min_index]]$node
    
    prune_node(node_to_prune)
    
    seq[[length(seq)+1]] <- clone_tree(current)
    alphas <- c(alphas, alpha)
    
    if (is_leaf(current))
      break
  }
  
  list(
    trees = seq,
    alphas = alphas
  )
}


select_tree_lambda <- function(sequence, lambda, y) {
  
  best_score <- Inf
  best_tree <- NULL
  
  for (i in seq_along(sequence$trees)) {
    
    tree <- sequence$trees[[i]]
    
    R <- subtree_risk(tree, y)
    size <- num_leaves(tree)
    
    score <- R + lambda * size
    
    # cat("Tree", i,
    #     "| leaves =", size,
    #     "| Risk =", R,
    #     "| Score =", score, "\n")
    
    if (score < best_score) {
      best_score <- score
      best_tree <- tree
    }
  }
  best_tree
}

cart_prune_ccp <- function(tree, y, lambda) {

  seq <- cost_complexity_sequence(tree, y)
  best <- select_tree_lambda(seq, lambda, y)

  list(
    sequence = seq,
    optimal_tree = best
  )
}