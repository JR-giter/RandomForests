# generating a boootstrap sample out of our data set
bootstrap_sample <- function(dataSet) {
  n <- nrow(dataSet)
  idx <- sample(seq_len(n), size = n, replace = TRUE) # ziehen mit zurücklegen
  dataSet[idx, , drop = FALSE]
}

# generating "n_bootstrapSamples" out of the first
bagging_greedycart <- function(data, n_bootstrapSamples, properties, n_nodes, mode, target) {

  # empty list to store each tree
  models <- vector("list", n_bootstrapSamples)

  # fill "models" with bootstrapsamples
  for (b in seq_len(n_bootstrapSamples)) {
    # generate one bootstrap sample
    boot <- bootstrap_sample(data)
    # generate model/ tree out of "boot"
    models[[b]] <- generate_cart_tree(
      dataSet = boot,
      properties = properties,
      n_nodes = n_nodes,
      mode = mode,
      target = target
    )
  }

  models
}

# =============================================================
# Tests
# =============================================================

# testing bagging_trees
test_bagging <- function(models, dataPoints, mode, target) {
  # Extract features used by the first tree
  X <- as.matrix(dataPoints[, models[[1]]$properties, drop = FALSE])

  # Get actual target
  y <- dataPoints[[target]]

  # Initialize matrix to store predictions
  pred_matrix <- matrix(NA, nrow = nrow(X), ncol = length(models))

  # Loop over each model
  for (i in seq_along(models)) {
    tree <- models[[i]]
    pred_matrix[, i] <- apply(X, 1, function(row) predict_cart(tree, row))
  }

  # Bagged predictions (average for regression)
  bagged_preds <- rowMeans(pred_matrix)

  # Create result data frame
  result <- data.frame(actual = y, prediction = bagged_preds)

  # Calculate delta for regression or classification
  if (mode == "regression") {
    result$delta <- abs(y - bagged_preds) / y * 100
  }
  else (mode == "classification") {
    result$delta <- ifelse(result$actual == result$prediction, 0, 100)
    attr(result, "accuracy") <- mean(result$delta == 0) * 100
  }

  return(result)
}

