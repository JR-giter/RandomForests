# load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# loading own scripts
source("Preprocessing.R")
source("Greedy_Cart.R")
source("Plotting.R")
source("Bagging.R")
source("Pruning.R")
source("RandomForrest.R")

# =============================================================
# SECTION: Loading Testing Data
# =============================================================

#install.packages("AmesHousing")
library(AmesHousing)
ames<- make_ames()


# =============================================================
# SECTION: Different tests and applications based of ames
# =============================================================

# TEST 1: Regression Tree 
# DONE
{
  # generating cart tree
  cart_tree <- generate_cart_tree( dataSet = ames, 
                                   n_properties = 5, 
                                   n_nodes = 600, 
                                   mode = "regression",
                                   target = "Sale_Price")
  
  # generating tests + results
  test_results <- test_cart( tree = cart_tree,
                             dataPoints = ames[720:730,], 
                             mode = "regression",
                             target = "Sale_Price")
  
  # printing + plotting results
  show_results(test_results)
}

# TEST 2: Classification Tree
# TODO
{
  
}

# TEST 3: Pruning
# TODO
{
  # generating cart tree
  cart_tree <- generate_cart_tree( dataSet = ames, 
                                   n_properties = 20, 
                                   n_nodes = 50, 
                                   mode = "regression",
                                   target = "Sale_Price")
  # Visualizing fully grown Tree
  plot_cart_tree(cart_tree)
  
  # Defining Set of Lambdas, and Inputvariables 
  lambdas <- seq(0, 100, length.out = 100)
  rm(lambdas)
  # Pruning Algorithm and Visualization
  pruned_tree <- prune_tree(cart_tree, K = 5)
  plot_cart_tree(pruned_tree$optimal_tree)
  
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
{
  # generating bagging models
  models <- bagging_greedycart( data = ames,
                                n_bootstrapSamples = 20,
                                n_properties = 20,
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
{
  # 1. Load data
  train_data <- ames[1:100, ]
  test_data  <- ames[110:120, ]
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
    ProError  = (abs(test_data$Sale_Price - predictions) / test_data$Sale_Price) * 100
  )
  
  print(results)
}

