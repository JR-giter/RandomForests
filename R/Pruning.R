####
# Pruning
####

# Helper Functions
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

# Risk Calculation with mode as argument. For (sub)trees and then nodes on recursion end.
node_risk <- function(node, y, mode = "regression") {
  idx <- node$indices
  if (length(idx) == 0) return(0)

  pred <- node$prediction

  if (mode == "regression") {
    return(sum((y[idx] - pred)^2))
  } else {
    return(sum(y[idx] != pred))
  }
}

subtree_risk <- function(node, y, mode = "regression") {
  if (is_leaf(node))
    return(node_risk(node, y, mode))

  subtree_risk(node$left_child, y, mode) +
    subtree_risk(node$right_child, y, mode)
}


# Weakest Link Computation. Here noted with "g" as variable.
compute_g <- function(node, y, mode = "regression", result = list()) {
  if (is_leaf(node))
    return(result)

  Rt  <- node_risk(node, y, mode)
  RTt <- subtree_risk(node, y, mode)
  leaves <- num_leaves(node)

  # Easier Form of 6.15 (Definition inside the Pruning Chapter)
  g <- (Rt - RTt) / (leaves - 1)

  result[[length(result) + 1]] <- list(
    node = node,
    g = g
  )

  result <- compute_g(node$left_child, y, mode, result)
  result <- compute_g(node$right_child, y, mode, result)

  result
}

# Prediciton functions
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

# Actual Pruning and Sequencing
prune_node <- function(node) {
  node$left_child  <- NULL
  node$right_child <- NULL
  node$split_feature_j <- NULL
  node$split_value_i   <- NULL
}

cost_complexity_sequence <- function(tree, y, mode = "regression") {
  trees  <- list()
  alphas <- c()

  current <- clone_tree(tree)
  trees[[1]] <- clone_tree(current)

  repeat {
    g_list <- compute_g(current, y, mode)

    if (length(g_list) == 0) break

    g_vals <- sapply(g_list, function(x) x$g)
    k <- which.min(g_vals)
    alpha <- g_vals[k]

    prune_node(g_list[[k]]$node)

    trees[[length(trees) + 1]] <- clone_tree(current)
    alphas <- c(alphas, alpha)

    if (is_leaf(current)) break
  }

  list(trees = trees, alphas = alphas)
}

# Select the best Tree with given Lambda
select_tree_lambda <- function(seq, lambda, y, mode = "regression") {
  best <- seq$trees[[1]]
  best_score <- Inf

  for (tree in seq$trees) {
    R <- subtree_risk(tree, y, mode)
    size <- num_leaves(tree)

    # Cost-complexity score: R(T) + lambda * |T|
    score <- R + lambda * size

    if (score < best_score) {
      best_score <- score
      best <- tree
    }
  }
  best
}

# Main Prune function need to prune a CART.

#' Prune a CART Tree using Cost-Complexity Pruning
#'
#' This function performs cost-complexity pruning on a fully grown CART tree.
#' It generates a sequence of subtrees by iteratively removing the "weakest link"
#' and uses K-fold cross-validation to select the optimal complexity parameter (lambda).
#' The final output is the subtree that minimizes the cross-validated error.
#'
#' @param tree A root node object of a fully grown CART tree.
#' @param lambdas A numeric vector of complexity parameters to evaluate. If
#'   \code{NULL} (default), the function automatically extracts the unique
#'   critical values from the tree's cost-complexity sequence.
#' @param K An integer specifying the number of folds for cross-validation.
#'   Default is 5.
#' @param mode A character string indicating the type of tree: either
#'   \code{"regression"} or \code{"classification"}. Default is \code{"regression"}.
#'
#' @return Returning a list containing:
#' \itemize{
#'   \item \code{optimal_tree}: The pruned tree object that performed best in CV.
#'   \item \code{best_lambda}: The complexity parameter \code{lambda} that minimized the CV error.
#'   \item \code{cv_values}: A numeric vector of the average cross-validation errors for each lambda.
#'   \item \code{lambdas}: The vector of lambda values used during the process.
#'   \item \code{sequence}: The full cost-complexity sequence of trees and their corresponding alphas.
#' }
#'
#' @export
prune_tree <- function(tree, lambdas = NULL, K = 5, mode = "regression") {
  X <- tree$X
  y <- tree$y
  n <- length(y)

  # Helper to rebuild tree based on mode
  build_tree_from_indices <- function(indices) {
    Xsub <- X[indices, , drop = FALSE]
    ysub <- y[indices]

    if (mode == "regression") {
      new_tree <- greedy_cart_regression(Xsub, ysub)
    } else {

      new_tree <- greedy_cart_classification(Xsub, ysub)
    }

    new_tree$X <- Xsub
    new_tree$y <- ysub
    new_tree
  }

  # Folds
  shuffled <- sample(1:n)
  folds <- split(shuffled, cut(seq_along(shuffled), K, labels = FALSE))

  # Collect lambdas from the full tree if not provided
  if (is.null(lambdas)) {
    full_seq <- cost_complexity_sequence(tree, y, mode)
    lambdas <- unique(full_seq$alphas)
  }

  cv_error <- rep(0, length(lambdas))

  # Cross Validation Loop
  for (k in seq_along(folds)) {
    val_idx <- folds[[k]]
    train_idx <- setdiff(1:n, val_idx)

    train_tree <- build_tree_from_indices(train_idx)
    seq <- cost_complexity_sequence(train_tree, train_tree$y, mode)

    for (i in seq_along(lambdas)) {
      lambda <- lambdas[i]
      pruned <- select_tree_lambda(seq, lambda, train_tree$y, mode)

      preds <- predict_tree(pruned, X, val_idx)

      if (mode == "regression") {
        err <- mean((y[val_idx] - preds)^2)
      } else {
        err <- mean(y[val_idx] != preds)
      }
      cv_error[i] <- cv_error[i] + err
    }
  }

  cv_error <- cv_error / K
  best_index <- which.min(cv_error)
  best_lambda <- lambdas[best_index]

  # Final pruning of full tree
  full_seq <- cost_complexity_sequence(tree, y, mode)
  optimal_tree <- select_tree_lambda(full_seq, best_lambda, y, mode)

  list(
    optimal_tree = optimal_tree,
    best_lambda = best_lambda,
    cv_values = cv_error,
    lambdas = lambdas,
    sequence = full_seq
  )
}
