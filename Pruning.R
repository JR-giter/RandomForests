# Pruning cuts unnecessary splits from a decision tree after training.
# This prevents overfitting and improves prediction accuracy on new data.
# It also makes the model smaller and easier to understand.

############################################################
## COST-COMPLEXITY PRUNING FOR GREEDY CART TREES
############################################################

############################################################
## BASIC TREE UTILITIES
############################################################

is_leaf <- function(node) {
  is.null(node$split_feature_j)
}

get_leaves <- function(node) {
  if (is_leaf(node)) return(list(node))
  c(get_leaves(node$left_child), get_leaves(node$right_child))
}

count_leaves <- function(node) {
  length(get_leaves(node))
}

clone_subtree <- function(node) {
  if (is_leaf(node)) {
    return(new_node(node$indices, node$prediction))
  }
  new_node(
    node$indices,
    node$prediction,
    node$split_feature_j,
    node$split_value_i,
    clone_subtree(node$left_child),
    clone_subtree(node$right_child)
  )
}

############################################################
## NODE RISK FUNCTIONS
############################################################

node_risk_regression <- function(node, y) {
  if (length(node$indices) == 0) return(0)
  mu <- mean(y[node$indices])
  sum((y[node$indices] - mu)^2)
}

node_risk_classification <- function(node, y) {
  if (length(node$indices) == 0) return(0)
  tab <- table(y[node$indices])
  length(node$indices) - max(tab)
}

subtree_risk <- function(node, y, type=c("regression","classification")) {
  type <- match.arg(type)
  if (is_leaf(node)) {
    if (type=="regression") return(node_risk_regression(node,y))
    return(node_risk_classification(node,y))
  }
  subtree_risk(node$left_child,y,type) + subtree_risk(node$right_child,y,type)
}

############################################################
## COST COMPLEXITY MEASURE g(t)
############################################################

compute_alpha_node <- function(node, y, type=c("regression","classification")) {
  type <- match.arg(type)
  if (is_leaf(node)) return(Inf)
  
  Rt <- if (type=="regression") node_risk_regression(node,y)
  else node_risk_classification(node,y)
  
  RTt <- subtree_risk(node,y,type)
  leaves <- count_leaves(node)
  
  if (leaves <= 1) return(Inf)
  (Rt - RTt) / (leaves - 1)
}

collect_internal_nodes <- function(node) {
  if (is_leaf(node)) return(list())
  c(list(node), collect_internal_nodes(node$left_child), collect_internal_nodes(node$right_child))
}

############################################################
## WEAKEST LINK PRUNING STEP
############################################################

prune_weakest_link <- function(tree, y, type=c("regression","classification")) {
  type <- match.arg(type)
  nodes <- collect_internal_nodes(tree)
  alphas <- sapply(nodes, compute_alpha_node, y=y, type=type)
  
  k <- which.min(alphas)
  prune_node <- nodes[[k]]
  
  # replace subtree by leaf
  prune_node$split_feature_j <- NULL
  prune_node$split_value_i <- NULL
  prune_node$left_child <- NULL
  prune_node$right_child <- NULL
}

############################################################
## GENERATE SUBTREE SEQUENCE T^(0)...T^(P)
############################################################

generate_pruning_sequence <- function(tree, y, type=c("regression","classification")) {
  type <- match.arg(type)
  seq <- list(clone_subtree(tree))
  
  repeat {
    current <- clone_subtree(seq[[length(seq)]])
    if (is_leaf(current)) break
    
    prune_weakest_link(current,y,type)
    seq[[length(seq)+1]] <- current
    
    if (count_leaves(current)==1) break
  }
  seq
}

############################################################
## COST COMPLEXITY FUNCTION
############################################################

cost_complexity <- function(tree, y, alpha, type=c("regression","classification")) {
  type <- match.arg(type)
  subtree_risk(tree,y,type) + alpha * count_leaves(tree)
}

############################################################
## K-FOLD CROSS VALIDATION FOR ALPHA SELECTION
############################################################

select_alpha_cv <- function(tree_builder, X, y, alphas, K=5, type=c("regression","classification")) {
  type <- match.arg(type)
  n <- nrow(X)
  folds <- sample(rep(1:K,length.out=n))
  
  cv_err <- rep(0,length(alphas))
  
  for (k in 1:K) {
    train <- which(folds!=k)
    test  <- which(folds==k)
    
    tree <- tree_builder(X[train,,drop=FALSE],y[train])
    seq <- generate_pruning_sequence(tree,y[train],type)
    
    for (i in seq_along(alphas)) {
      trees <- seq
      costs <- sapply(trees,cost_complexity,y=y[train],alpha=alphas[i],type=type)
      best_tree <- trees[[which.min(costs)]]
      
      preds <- predict_tree(best_tree,X[test,,drop=FALSE])
      
      if (type=="regression")
        cv_err[i] <- cv_err[i] + mean((y[test]-preds)^2)
      else
        cv_err[i] <- cv_err[i] + mean(y[test]!=preds)
    }
  }
  
  alphas[which.min(cv_err)]
}

############################################################
## PREDICTION
############################################################

predict_tree <- function(node,X) {
  apply(X,1,function(row) predict_one(node,row))
}

predict_one <- function(node,x) {
  if (is_leaf(node)) return(node$prediction)
  if (x[node$split_feature_j] < node$split_value_i)
    predict_one(node$left_child,x)
  else
    predict_one(node$right_child,x)
}

