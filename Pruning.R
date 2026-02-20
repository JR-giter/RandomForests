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

