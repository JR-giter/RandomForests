library(dplyr)
library(AmesHousing)
library(tidyverse)
source("Preprocessing.R")
source("Greedy_Cart.R")
source("Plotting_Trees.R")



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
  #set.seed(1)
  #idx <- sample(seq_len(nrow(dataSet)), n_nodes)
  #dataSub <- dataSet[idx, ]
  
  #get the first n datapoints of the dataset
  dataSub <- dataSet[seq_len(n_nodes), ]
  
  
  
  
  
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
  
  
  # select target
  target_variable <- dataSub[[target]]

  
  if(mode == "regression") {
    tree <- greedy_cart_regression(input_matrix, target_variable)
    tree$properties <- properties
  }else if(mode == "classification") {
    tree <- greedy_cart_classification(input_matrix, target_variable)
  } else {
    stop("mode must be 'regression' or 'classification'")
  }
  
  return(tree)
}






# BASIC TESTING LOGIC

find_leaf <- function(tree, dataPoint) {
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
  return(node)
}



predict_cart <- function(tree, dataPoint) {
  leaf <- find_leaf(tree, dataPoint)
  return(leaf$prediction)
}








# TESTING OF MANY POINTS

test_cart <- function(tree, dataPoints, mode = 'regression', target = "Sale_Price") {
  
  # 1. Tatsächliche Werte
  actual <- dataPoints[[target]]
  
  # 2. WICHTIG: gleiche Properties verwenden wie beim Training
  reduced_data <- select_properties(dataPoints, tree$properties)
  
  # nur numerisch
  reduced_data <- select_numeric(reduced_data)
  
  # Matrix wie beim Training
  reduced_data <- as.matrix(reduced_data)
  
  # 3. Predictions
  preds <- apply(reduced_data, 1, function(row) {
    predict_cart(tree, row)
  })
  
  # 4. Klassifikation
  if (mode == "classification") {
    preds <- as.factor(preds)
  }
  
  # 5. Ergebnis
  result <- data.frame(
    actual = actual,
    prediction = preds,
    delta = abs(actual - preds)/actual * 100
  )
  
  return(result)
}






# 2. Baum trainieren (z.B. 50 Datenpunkte, 5 Features)
tree <- GreedyCart(
  dataSet = ames,
  n_properties = 40,
  n_nodes = 1500,
  mode = "regression",
  target = "Sale_Price"
)


# 3. Testdaten auswählen (z.B. Zeilen 51–60)
test_data <- ames[1500:2000, ]

# 4. Baum testen
results <- test_cart(
  tree = tree,
  dataPoints = test_data,
  mode = "regression",
  target = "Sale_Price"
)

# 5. Ausgabe
print(results)

c(
  max = max(results$delta, na.rm = TRUE),
  mean = mean(results$delta, na.rm = TRUE),
  median = median(results$delta, na.rm = TRUE)
)

results$delta <- as.numeric(gsub("%", "", results$delta))
hist(results$delta,
     breaks = 40,
     main = "Distribution of Percentage Delta",
     xlab = "Delta (%)",
     col = "steelblue",
     border = "white")


plot(density(results$delta),
     main = "Density of Percentage Delta",
     xlab = "Delta (%)",
     lwd = 2)
