# Pruning cuts unnecessary splits from a decision tree after training.
# This prevents overfitting and improves prediction accuracy on new data.
# It also makes the model smaller and easier to understand.

############################################################
## COST-COMPLEXITY PRUNING FOR GREEDY CART TREES
############################################################

############################################################
## TREE HELPERS
############################################################


is_leaf <- function(node) {
  is.null(node$split_feature_j)
}

get_leaves <- function(node) {
  if (is_leaf(node)) return(list(node))
  c(get_leaves(node$left_child),
    get_leaves(node$right_child))
}

count_leaves <- function(node) {
  length(get_leaves(node))
}

############################################################
## RISK OF A NODE (REGRESSION OR CLASSIFICATION)
############################################################

node_risk <- function(node, target_variable, type = "regression") {
  
  y <- target_variable[node$indices]
  
  if (type == "regression") {
    
    pred <- mean(y)
    return(sum((y - pred)^2))
    
  } else {
    
    majority <- names(which.max(table(y)))
    return(sum(y != majority))
  }
}

############################################################
## TOTAL TREE RISK
############################################################

tree_risk <- function(node, target_variable, type="regression") {
  
  leaves <- get_leaves(node)
  
  sum(sapply(leaves, function(l)
    node_risk(l, target_variable, type)))
}

############################################################
## COMPUTE ALPHA FOR EACH INTERNAL NODE
############################################################

compute_alpha_values <- function(node, target_variable, type="regression") {
  
  result <- list()
  
  recurse <- function(node) {
    
    if (is_leaf(node)) return()
    
    subtree_leaves <- get_leaves(node)
    
    if (length(subtree_leaves) > 1) {
      
      R_t <- node_risk(node, target_variable, type)
      
      R_Tt <- sum(
        sapply(subtree_leaves,
               function(l) node_risk(l, target_variable, type))
      )
      
      alpha <- (R_t - R_Tt) / (length(subtree_leaves) - 1)
      
      result[[length(result)+1]] <<- list(
        node=node,
        alpha=alpha
      )
    }
    
    recurse(node$left_child)
    recurse(node$right_child)
  }
  
  recurse(node)
  
  result
}

############################################################
## PRUNE NODE
############################################################

prune_node <- function(node, target_variable, type="regression") {
  
  y <- target_variable[node$indices]
  
  if (type=="regression")
    node$prediction <- mean(y)
  else
    node$prediction <- names(which.max(table(y)))
  
  node$split_feature_j <- NULL
  node$split_value_i <- NULL
  node$left_child <- NULL
  node$right_child <- NULL
}

############################################################
## COST COMPLEXITY PRUNING SEQUENCE
############################################################

cost_complexity_pruning <- function(root,
                                    target_variable,
                                    type="regression") {
  
  trees <- list()
  alphas <- c()
  
  current_tree <- root
  
  repeat {
    
    trees[[length(trees)+1]] <- current_tree
    
    alpha_candidates <-
      compute_alpha_values(current_tree,
                           target_variable,
                           type)
    
    if (length(alpha_candidates) == 0)
      break
    
    alpha_values <- sapply(alpha_candidates,
                           function(x) x$alpha)
    
    alpha_min <- min(alpha_values)
    
    alphas <- c(alphas, alpha_min)
    
    weakest_nodes <-
      alpha_candidates[alpha_values == alpha_min]
    
    for (entry in weakest_nodes)
      prune_node(entry$node,
                 target_variable,
                 type)
  }
  
  list(
    trees = trees,
    alphas = alphas
  )
}

############################################################
## SELECT TREE FOR GIVEN LAMBDA
############################################################

select_tree_lambda <- function(pruning_result,
                               lambda,
                               target_variable,
                               type="regression") {
  
  trees <- pruning_result$trees
  
  scores <- sapply(trees, function(tree) {
    
    tree_risk(tree, target_variable, type) +
      lambda * count_leaves(tree)
    
  })
  
  trees[[which.min(scores)]]
}



