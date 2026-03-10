# load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# =============================================================
# SECTION: Loading Testing Data
# =============================================================

#install.packages("AmesHousing")
library(AmesHousing)
# ames<- make_ames()

# =============================================================
# SECTION: Different tests and applications based of ames
# =============================================================

# TEST 1: Regression Tree
# DONE
test1 <- function(){
  # generating cart tree
  cart_tree <- generate_cart_tree( dataSet = ames,
                                   properties = 5,
                                   n_nodes = 100,
                                   mode = "regression",
                                   target = "Sale_Price")

  # generating tests + results
  test_results <- test_cart( tree = cart_tree,
                             dataPoints = ames[2900:2930,],
                             mode = "regression",
                             target = "Sale_Price")

  # printing + plotting results
  show_results(test_results)

}


# TEST 2: Classification Tree
# TODO
test2 <- function(){
  # ===Klassifikation High/Low ===

  median_price <- median(ames$Sale_Price)
  ames$AboveMedian <- ifelse(ames$Sale_Price > median_price, "High", "Low")

  cart_tree_class <- generate_cart_tree(
    dataSet = ames,
    properties = 5,
    n_nodes = 100,
    mode = "classification",
    target = "AboveMedian"
  )

  pruned_class <- prune_tree(cart_tree_class, K = 5)

  test_results_class <- test_cart(
    tree = cart_tree_class,
    dataPoints = ames[420:800,],
    mode = "classification",
    target = "AboveMedian"
  )

  print(test_results_class)
  cat("\nAccuracy:\n", attr(test_results_class, "accuracy"))
  cat("\nConfusion Matrix:\n")
  print(attr(test_results_class, "confusion_matrix"))


  # === Klassifikation OverallQual ===

  ames$QualClass <- factor(ames$Overall_Qual)
  ames$QualClass

  cart_tree_qual <- generate_cart_tree(
    dataSet = ames,
    properties = 15,
    n_nodes = 100,
    mode = "classification",
    target = "QualClass"
  )

  test_results_qual <- test_cart(
    tree = cart_tree_qual,
    dataPoints = ames[720:740,],
    mode = "classification",
    target = "QualClass"
  )

  print(test_results_qual)
  cat("\nAccuracy:\n", attr(test_results_qual, "accuracy"))
  cat("\nConfusion Matrix:\n")
  print(attr(test_results_qual, "confusion_matrix"))
}

# TEST 3: Pruning
# TODO
test3 <- function(){
  # generating cart tree
  cart_tree <- generate_cart_tree( dataSet = ames,
                                   properties = 34,
                                   n_nodes = 50,
                                   mode = "regression",
                                   target = "Sale_Price")

  # Visualizing fully grown Tree
  plot_cart_tree(cart_tree)

  pruned_tree <- prune_tree(cart_tree, K = 5)
  plot_cart_tree(pruned_tree$optimal_tree)

  test_results <- test_cart( tree = pruned_tree$optimal_tree,
                             dataPoints = ames[2900:2930,],
                             mode = "regression",
                             target = "Sale_Price")

  plot(pruned_tree$lambdas,
       pruned_tree$cv_values,
       type="l",
       log="x",
       xlab="lambda",
       ylab="CV error")
  abline(v=pruned_tree$best_lambda,col="red")
}

# TEST 4: Bagging
# TODO
test4 <- function(){
  # generating bagging models
  models <- bagging_greedycart( data = ames,
                                n_bootstrapSamples = 20,
                                properties = 20,
                                n_nodes = 100, # takes the first n data points out of data
                                target = "Sale_Price")

  # plotting each tree
  # for(i in seq_along(models)) {
  #   cat("Plotting tree", i, "\n")
  #   print(plot_cart_tree(models[[i]]))
  # }

  # generating tests + results
  test_results <- test_bagging(models = models,
                               dataPoints = ames[120:130,],
                               mode = "regression",
                               target = "Sale_Price")

  # printing + plotting results
  show_results(test_results)
}

# TEST 5: Random Forrests
test5 <- function(){
  # 1. Load data
  train_data <- ames[1:100, ]
  test_data  <- ames[2900:2930, ]
  # 2. Parameters
  B_trees <- 50
  m_features <- 8      # ncol(ames) = 81, 81-1 -> \sqrt(80) = 8
  target_var <- "Sale_Price"

  # 3. Train the Random Forest
  my_rf <- random_forest(
    data = train_data,
    B = B_trees,
    m = m_features,
    target = target_var,
    mode = "regression"
  )

  # 4. Predict
  predictions <- predict_rf(my_rf, test_data)

  # 5. Compare with Actuals
  results <- data.frame(
    Actual = test_data$Sale_Price,
    Predicted = predictions,
    Error = abs(test_data$Sale_Price - predictions),
    delta  = (abs(test_data$Sale_Price - predictions) / test_data$Sale_Price) * 100
  )

  show_results(results)
}

# properties input tests
otherfunction <- function(){
  # select properties by input
  cart_tree2 <- generate_cart_tree( dataSet = ames,
                                    properties = c(2, 34, 23, 5, 7),
                                    n_nodes = 10,
                                    mode = "regression",
                                    target = "Sale_Price")
  # Visualizing fully grown Tree
  plot_cart_tree(cart_tree2)
  cart_tree2$properties

  # select properties by name
  cart_tree3 <- generate_cart_tree( dataSet = ames,
                                    properties = c("Lot_Area", "Garage_Area"),
                                    n_nodes = 10,
                                    mode = "regression",
                                    target = "Sale_Price")
  # Visualizing fully grown Tree
  plot_cart_tree(cart_tree3)
  cart_tree3$properties
}
