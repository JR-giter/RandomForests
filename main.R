# This file represents a potential use case based on the <ames housing Data Set>


#install.packages("AmesHousing")

# used packages
library(AmesHousing)
library(dplyr)
library(tidyverse)
library(ggplot2)

# loading own scripts
source("Preprocessing.R")
source("Greedy_Cart.R")
source("Plotting_Trees.R")
source("AlgorithmApplication.R")

# =============================================================
# SECTION: Loading Testing Data
# =============================================================
ames<- make_ames()


# =============================================================
# SECTION: Different tests and applications
# =============================================================

# TEST 1: Cart Tree regression
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
