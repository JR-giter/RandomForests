# load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# loading own scripts
source("Preprocessing.R")
source("Greedy_Cart.R")
source("Plotting_Trees.R")
source("AlgorithmApplication.R")
source("bagging.R")

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
  cart_tree <- generate_cart_tree( data_set = ames, 
                                   number_properties = 20, 
                                   number_nodes = 100, 
                                   mode = "regression",
                                   target_variable = "Sale_Price")
  
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
  
}

# TEST 4: Bagging
# TODO
{
  # generating bagging models
  models <- bagging_greedycart( data = ames,
                                n_bootstrapSamples = 20,
                                n_properties = 20,
                                n_nodes = 400, # takes the first n data points out of data
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
