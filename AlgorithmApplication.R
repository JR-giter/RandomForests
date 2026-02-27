library(dplyr)
library(AmesHousing)
library(tidyverse)
source("Preprocessing.R")
source("Greedy_Cart.R")
source("Plotting_Trees.R")

# prepares data before using
prepare_data <- function(dataSet, n_properties, target_property, filter_mode = "numeric"){
  # filters out non-numeric properties if "filter_mode" -> numeric
  if(filter_mode == "numeric"){
    dataSet <- dataSet[, sapply(dataSet, is.numeric), drop = FALSE]
  }
  
  # rating properties
  rated_properties <-  attribute_rating_V1(dataSet, target_property)
  
  if (n_properties > length(rated_properties)) {
    stop("n_properties is larger than the number of available rated properties.") 
  }
  
  # selecting top "n_properties"
  properties <- list(rated_properties[1:n_properties])
  
  # cutting out unnecessary properties
  props <- unlist(properties, use.names = FALSE)
  
  # Ensure target property is always included
  props <- unique(c(target_property, props))
  
  missing <- setdiff(props, names(dataSet))
  
  if (length(missing) > 0) {
    stop('Propertie does not exist in DataSet')
  }
  
  # returning data as well as selected properties
  return(list(
    data = dataSet[, props, drop = FALSE],
    properties = props
  ))
}



# generating a greedy Cart Tree
GreedyCart <-  function(dataSet, n_properties, n_nodes, mode = "regression", target = "Sale_Price"){
  
  # get the Data-set as DataFrame
  dataSet <- as.data.frame(dataSet)

  # get the first n datapoints of the dataset
  dataSub <- dataSet[seq_len(n_nodes), ]
  
  # reducing data based on n_properties (*non-numeric)
  prep <- prepare_data(dataSet = dataSub, 
                       n_properties = n_properties, 
                       target_property = target)
  # selecting data
  reduced_data <- prep$data
  
  # create Matrix out of reduced_data
  input_matrix <- as.matrix(reduced_data)
  
  # get "target_variable"
  target_variable <- dataSub[[target]]
  
  if(mode == "regression") {
    tree <- greedy_cart_regression(input_matrix, target_variable)
    tree$properties <- prep$properties
  }else if(mode == "classification") {
    tree <- greedy_cart_classification(input_matrix, target_variable)
  } else {
    stop("mode must be 'regression' or 'classification'")
  }
  
  return(tree)
}

#### TESTING FUNCTIONS

# finding prediction result for single "dataPoint" inside "tree"
predict_cart <- function(tree, dataPoint) {
  node <- tree
  
  while(!(is.null(node$split_feature_j))) {
    
    j <- node$split_feature_j
    s <- node$split_value_i
    
    if (dataPoint[j] < s) {
      node <- node$left_child
    } else {
      node <- node$right_child
    }
  }
  
  return(node$prediction)
}

# testing  a cart tree on a number of input dataPoints 
test_cart <- function(tree, dataPoints, mode = "regression", target = "Sale_Price") {
  
  # filtering out only necessary properties
  X <- as.matrix(dataPoints[, tree$properties, drop = FALSE])
  # getting actual target
  y <- dataPoints[[target]]
  
  # finding prediction for each dataPoint
  preds <- apply(X, 1, function(row) predict_cart(tree, row))
  
  # storing result
  result <- data.frame(actual = y, prediction = preds)
  
  # calculating delta for regression
  if (mode == "regression"){
    result$delta <- abs(y - preds) / y * 100
  }
  
  return(result)
}

# generating a cart tree based on dataset,...
generate_cart_tree <- function(data_set, number_properties, number_nodes, mode, target_variable){
  tree <- GreedyCart(
    dataSet = data_set,
    n_properties = number_properties,
    n_nodes = number_nodes,
    mode = mode,
    target = target_variable
  )
  
  return(tree)
}

# visualizing results
show_results <- function(test_results){
  # comparing test results to actual results 
  print(test_results)
  
  print(
    c(
      max = max(test_results$delta, na.rm = TRUE),
      mean = mean(test_results$delta, na.rm = TRUE),
      median = median(test_results$delta, na.rm = TRUE)
    )
  )
  
  # plotting results
  
  hist(test_results$delta,
       breaks = 40,
       main = "Distribution of Percentage Delta",
       xlab = "Delta (%)",
       col = "steelblue",
       border = "white")
  
  plot(density(test_results$delta),
       main = "Density of Percentage Delta",
       xlab = "Delta (%)",
       lwd = 2)
}

##### ACTUAL TESTING 
{
  # generating cart tree
  cart_tree <- generate_cart_tree(data_set = ames, 
                                  number_properties = 20, 
                                  number_nodes = 100, 
                                  mode = "regression",
                                  target_variable = "Sale_Price")
  
  
  # generating tests + results
  test_results <- test_cart(
    tree = cart_tree,
    dataPoints = ames[120:130,], 
    mode = "regression",
    target = "Sale_Price"
  )
  
  # printing + plotting results
  show_results(test_results)
}







