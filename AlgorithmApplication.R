library(dplyr)
library(AmesHousing)
library(tidyverse)
source("Preprocessing.R")


select_properties <- function(dataSet, properties) {
  
  props <- unlist(properties, use.names = FALSE)
  
  missing <- setdiff(props, names(dataSet))
  
  if (length(missing) > 0) {
    stop('Propertie does not exist in DataSet')
  }
  
  dataSet[, props, drop = FALSE]
}



select_numeric <- function(dataSet) {
  dataSet[, sapply(dataSet, is.numeric), drop = FALSE]
}



GreedyCart <-  function(dataSet, n_properties, n_nodes, mode = "regression", target = "Sale_Price"){
  
  
  # get the Data-set as DataFrame
  dataSet <- as.data.frame(dataSet)
  
  
  # get n datapoints out of dataSet (slider)
  set.seed(1)
  idx <- sample(seq_len(nrow(dataSet)), n_nodes)
  dataSub <- dataSet[idx, ]
  
  
  
  # get best properties
  rated <-  attribute_rating_V1(dataSet, target)
  
  if (n_properties > length(rated)) {
    stop("n_properties is larger than the number of available rated properties.") 
  }
  
  properties <-  list(rated[1:n_properties])

  

  
  # select properties
  reduced_data <- select_properties(dataSub, properties)

  # only select numeric data
  reduced_data <- select_numeric(reduced_data)

  # create Matrix
  input_matrix <- as.matrix(reduced_data)
  print(input_matrix)
  print(unique(input_matrix[,1]))
  
  
  # select target
  target_variable <- dataSub[[target]]

  
  if(mode == "regression") {
    tree <- greedy_cart_regression(input_matrix, target_variable)
  }else if(mode == "classification") {
    tree <- greedy_cart_classification(input_matrix, target_variable)
  } else {
    stop("mode must be 'regression' or 'classification'")
  }
  
  return(tree)
}

