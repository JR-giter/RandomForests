####
# Random Forrest Regression
#####
rf_tree_regression <- function(X, y, m) {
  d <- ncol(X)
  n <- nrow(X)

  root <- new_node(indices = 1:n, prediction = mean(y))

  repeat {
    best_risk <- Inf
    best_split <- NULL

    search_best_split <- function(node) {
      if (length(node$indices) <= 1 || !is.null(node$split_feature_j)) return()

      S <- sample(1:d, size = min(m, d), replace = FALSE)

      for (j in S) {
        current_feat <- X[node$indices, j]
        vals <- sort(unique(current_feat))
        if (length(vals) < 2) next

        split_points <- (vals[-1] + vals[-length(vals)]) / 2

        for (s in split_points) {
          left_mask <- current_feat < s
          idx_left <- node$indices[left_mask]
          idx_right <- node$indices[!left_mask]

          if (length(idx_left) == 0 || length(idx_right) == 0) next

          c1 <- mean(y[idx_left])
          c2 <- mean(y[idx_right])

          risk <- sum((y[idx_left] - c1)^2) + sum((y[idx_right] - c2)^2)

          if (risk < best_risk) {
            best_risk <<- risk
            best_split <<- list(node = node, j = j, s = s,
                                idx_l = idx_left, idx_r = idx_right,
                                c1 = c1, c2 = c2)
          }
        }
      }
    }

    find_leaves <- function(node) {
      if (is.null(node$split_feature_j)) {
        search_best_split(node)
      } else {
        find_leaves(node$left_child)
        find_leaves(node$right_child)
      }
    }

    find_leaves(root)
    if (is.null(best_split)) break

    n_ptr <- best_split$node
    n_ptr$split_feature_j <- best_split$j
    n_ptr$split_value_i   <- best_split$s
    n_ptr$left_child  <- new_node(best_split$idx_l, best_split$c1)
    n_ptr$right_child <- new_node(best_split$idx_r, best_split$c2)
  }
  return(root)
}
####
# Random Forrest Classification
####
rf_tree_classification <- function(X, y, m) {
  d <- ncol(X)
  n <- nrow(X)

  get_majority <- function(target) {
    if (length(target) == 0) return(NULL)
    tab <- table(target)
    names(tab)[which.max(tab)]
  }

  root <- new_node(indices = 1:n, prediction = get_majority(y))

  repeat {
    best_risk <- Inf
    best_split <- NULL

    search_best_split <- function(node) {

      if (length(node$indices) <= 1 || !is.null(node$split_feature_j)) return()

      S <- sample(1:d, size = min(m, d), replace = FALSE)

      for (j in S) {
        current_feat <- X[node$indices, j]
        vals <- sort(unique(current_feat))
        if (length(vals) < 2) next

        split_points <- (vals[-1] + vals[-length(vals)]) / 2

        for (s in split_points) {
          left_mask <- current_feat < s
          idx_left <- node$indices[left_mask]
          idx_right <- node$indices[!left_mask]

          if (length(idx_left) == 0 || length(idx_right) == 0) next

          y_l <- y[idx_left]
          y_r <- y[idx_right]

          c1 <- get_majority(y_l)
          c2 <- get_majority(y_r)

          risk <- sum(y_l != c1) + sum(y_r != c2)

          if (risk < best_risk) {
            best_risk <<- risk
            best_split <<- list(node = node, j = j, s = s,
                                idx_l = idx_left, idx_r = idx_right,
                                c1 = c1, c2 = c2)
          }
        }
      }
    }

    traverse <- function(node) {
      if (is.null(node$split_feature_j)) {
        search_best_split(node)
      } else {
        traverse(node$left_child)
        traverse(node$right_child)
      }
    }

    traverse(root)
    if (is.null(best_split)) break

    n_ptr <- best_split$node
    n_ptr$split_feature_j <- best_split$j
    n_ptr$split_value_i   <- best_split$s
    n_ptr$left_child  <- new_node(best_split$idx_l, best_split$c1)
    n_ptr$right_child <- new_node(best_split$idx_r, best_split$c2)
  }
  return(root)
}

#####
# Actualy Function for Random Forrests
####

#' Train a Random Forest Model
#'
#' This function implements a Random Forest for both regression and classification.
#' It builds an ensemble of decision trees using bootstrap samples and feature
#' subsetting (at each split, only a random subset of \code{m} features is considered).
#'
#' @param data A data frame containing the features and the target variable.
#' @param target A character string specifying the name of the target column.
#' @param B An integer representing the number of trees to grow in the forest.
#' @param m An integer representing the number of features to randomly select
#'   at each node split.
#' @param mode A character string, either \code{"regression"} or \code{"classification"},
#'   to determine the type of forest. Default is \code{"regression"}.
#'
#' @return A list of class "random_forest" containing:
#' \itemize{
#'   \item \code{trees}: A list of \code{B} trained tree objects.
#'   \item \code{mode}: The specified mode (regression or classification).
#'   \item \code{target}: The name of the target variable.
#' }
#'
#' @export
random_forest <- function(data, target, B, m, mode = "regression") {

  if (mode == "regression") {
    clean_data <- data[, sapply(data, is.numeric), drop = FALSE]
  } else {

    num_cols <- sapply(data, is.numeric)
    clean_data <- data[, num_cols | names(data) == target, drop = FALSE]
  }

  feature_names <- setdiff(names(clean_data), target)
  X <- as.matrix(clean_data[, feature_names])
  y <- clean_data[[target]]

  n <- nrow(X)
  d <- ncol(X)

  forest <- vector("list", B)
  # Bootstrapping
  for (b in 1:B) {

    boot_idx <- sample(1:n, size = n, replace = TRUE)
    X_boot <- X[boot_idx, , drop = FALSE]
    y_boot <- y[boot_idx]

    if (mode == "regression") {
      forest[[b]] <- rf_tree_regression(X_boot, y_boot, m)
    } else {
      forest[[b]] <- rf_tree_classification(X_boot, y_boot, m)
    }

    forest[[b]]$feature_names <- feature_names
  }

  return(list(trees = forest, mode = mode, target = target))
}

# Predicition Function specifically for Random Forrests

#' Predict with a Random Forest Model
#'
#' Generates predictions for new data using a trained Random Forest model.
#' For regression, it averages the predictions of all trees. For classification,
#' it uses majority voting.
#'
#' @param rf_model A list object returned by the \code{random_forest} function.
#' @param newData A data frame containing the features for which predictions
#'   are to be made.
#'
#' @return A vector of predictions. Numerical values are returned for
#'   regression, and character/factor labels for classification.
#'
#' @export
predict_rf <- function(rf_model, newData) {
  B <- length(rf_model$trees)
  n_samples <- nrow(newData)

  all_preds <- matrix(NA, nrow = n_samples, ncol = B)

  for (b in 1:B) {
    tree <- rf_model$trees[[b]]
    X_test <- as.matrix(newData[, tree$feature_names, drop = FALSE])

    all_preds[, b] <- apply(X_test, 1, function(row) predict_cart(tree, row))
  }

  if (rf_model$mode == "regression") {
    # Mean for Regression
    return(rowMeans(matrix(as.numeric(all_preds), nrow = n_samples)))
  } else {
    # Majority Vote for Classification
    final_labels <- apply(all_preds, 1, function(row) {
      tab <- table(row)
      names(tab)[which.max(tab)]
    })
    return(final_labels)
  }
}

#' Test and Evaluate Random Forest Performance
#'
#' @param rf_model The trained model from random_forest()
#' @param test_data Dataframe containing the test features and target
#' @return A dataframe with actuals, predictions, and error metrics
test_rf <- function(rf_model, test_data) {
  y_actual <- test_data[[rf_model$target]]

  preds <- predict_rf(rf_model, test_data)

  if (rf_model$mode == "regression") {
    delta <- abs(y_actual - preds) / (y_actual + 1e-10) * 100

    summary_stats <- c(
      max    = max(delta, na.rm = TRUE),
      min    = min(delta, na.rm = TRUE),
      mean   = mean(delta, na.rm = TRUE),
      median = median(delta, na.rm = TRUE)
    )
    return(summary_stats)

  } else {
    # Classification Mode
    is_correct <- y_actual == preds

    result <- data.frame(
      actual = y_actual,
      prediction = preds,
      correct = is_correct
    )

    # Summary Metrics
    accuracy <- mean(is_correct) * 100
    attr(result, "accuracy") <- accuracy

    cat(paste0("\n--- Classification Results ---\nAccuracy: ",
               round(accuracy, 2), "%\n"))

    # Print Confusion Matrix
    print(table(Actual = y_actual, Predicted = preds))

    return(result)
  }
}
