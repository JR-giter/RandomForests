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
source("FindBestLambda.R")


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
                                   n_properties = 20, 
                                   n_nodes = 100, 
                                   mode = "regression",
                                   target = "Sale_Price")
  
  # generating tests + results
  test_results <- test_cart( tree = cart_tree,
                             dataPoints = ames[120:130,], 
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
                                   n_nodes = 100, 
                                   mode = "regression",
                                   target = "Sale_Price")
  # Visualizing fully grown Tree
  plot_cart_tree(cart_tree)
  
  # Defining Set of Lambdas, and Inputvariables 
  lambdas <- seq(0, 1, length.out = 100)
  cart_tree$X <- cart_tree$properties
  cart_tree$Y <- "Sale_Price"
  
  # Pruning Algorithm
  pruned_tree <- find_best_lambda(cart_tree, lambdas, K = 5)
  plot_cart_tree(pruned_tree)
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
