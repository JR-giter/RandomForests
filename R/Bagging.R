#' Generate Bootstrap Samples
#' @keywords internal
bootstrap_sample <- function(dataSet) {
  n <- nrow(dataSet)
  idx <- sample(seq_len(n), size = n, replace = TRUE) # ziehen mit zurücklegen
  dataSet[idx, , drop = FALSE]
}

#' Bagging Greedy CART
#' @param data dataset
#' @param n_bootstrapSamples number of bootstrap samples
#' @param properties number of properties
#' @param n_nodes number of nodes
#' @param mode regression or classification
#' @param target target column
#' @return list of trees
#' @examples
#' bagging_greedycart(data = ames, n_bootstrapSamples = 20, properties = 20,
#' n_nodes = 100, mode = "regression", target = "Sale_Price")
#' @export
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

#' Testing Bagging Results
#' @param models bagging models
#' @param dataPoints test dataset
#' @param mode regression or classification
#' @param target target column
#' @return data.frame with actual/predicted
#' @examples
#' test_bagging(models = models, dataPoints = ames[1:10,], mode="regression",
#' target="Sale_Price")
#' @export
test_bagging <- function(models, dataPoints, mode, target) {

  # Extract features used by the first tree
  X <- as.matrix(dataPoints[, models[[1]]$properties, drop = FALSE])

  # Get Actual target
  y <- dataPoints[[target]]

  # Prediction matrix
  pred_matrix <- matrix(NA, nrow = nrow(X), ncol = length(models))

  for (i in seq_along(models)) {
    tree <- models[[i]]
    pred_matrix[, i] <- apply(X, 1, function(row) predict_cart(tree, row))
  }


  # REGRESSION
  if (mode == "regression") {

    bagged_preds <- rowMeans(pred_matrix)

    result <- data.frame(
      actual = y,
      prediction = bagged_preds,
      delta = abs(y - bagged_preds) / y * 100
    )

    return(result)
  }


  # CLASSIFICATION (Mehrheitsvoting)
  if (mode == "classification") {

    bagged_preds <- apply(pred_matrix, 1, function(row) {
      names(sort(table(row), decreasing = TRUE))[1]
    })

    result <- data.frame(
      actual = y,
      prediction = bagged_preds,
      delta = ifelse(y == bagged_preds, 0, 100)
    )

    attr(result, "accuracy") <- mean(y == bagged_preds) * 100

    return(result)
  }
}


