
# =============================================================
# Use of Greedy Cart
# =============================================================

# prepares data before using
prepare_data <- function(dataSet, properties, target, filter_mode = "numeric"){
  
  # filters out non-numeric properties if "filter_mode" -> numeric
  if (filter_mode == "numeric") {
    keep_cols <- sapply(dataSet, is.numeric)
    dataSet <- dataSet[, keep_cols, drop = FALSE]
  }
  
  
  # Determine selected properties based on input type
  if (is.numeric(properties) && length(properties) == 1) {
    
    # Case A: single number → top-n rated properties
    rated_properties <- attribute_rating(dataSet, target)
    
    if (properties > length(rated_properties)) {
      stop("Requested number of properties exceeds available rated properties.")
    }
    
    props <- rated_properties[1:properties]
    
    # Case B: integer vector → treat as column indices
  } else if (is.numeric(properties)) {
    if (any(properties < 1 | properties > ncol(dataSet))) {
      stop("Some indices in 'properties' are out of bounds.")
    }
    props <- names(dataSet)[properties]
    
    # Case C: character vector → treat as column names
  } else if (is.character(properties)) {
    missing <- setdiff(properties, names(dataSet))
    if (length(missing) > 0) {
      stop(paste("These properties do not exist in the dataset:", 
                 paste(missing, collapse = ", ")))
    }
    props <- properties
    
  } else {
    stop("Argument 'properties' must be either: numeric scalar, numeric vector, or character vector.")
  }
  
  
  # Return reduced dataset
  return(list(
    data = dataSet[, props, drop = FALSE],
    properties = props
  ))
}



# generating a greedy Cart Tree
generate_cart_tree <-  function(dataSet, properties, n_nodes, mode, target){
  
  # get the Data-set as DataFrame
  dataSet <- as.data.frame(dataSet)
  
  # get the first n datapoints of the dataset
  dataSub <- dataSet[seq_len(n_nodes), ]
  
  # reducing data based on n_properties (*non-numeric)
  prep <- prepare_data(dataSet = dataSub, 
                       properties = properties, 
                       target = target,
                       filter_mode = "numeric")
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
    tree$properties <- prep$properties
  } else {
    stop("mode must be 'regression' or 'classification'")
  }
  
  tree$X <- input_matrix
  tree$y <- target_variable
  return(tree)
}



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
test_cart <- function(tree, dataPoints, mode = "regression", target) {
  
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
  if (mode == "classification"){
    result$correct <- result$actual == result$prediction
    accuracy <- mean(result$correct)
    confusion <- table(result$actual, result$prediction)
    
    attr(result, "accuracy") <- accuracy
    attr(result, "confusion_matrix") <- confusion
  }
  
  return(result)
}










# =============================================================
# Tree Structure
# =============================================================



new_node <- function(indices, prediction,
                     split_feature_j = NULL,
                     split_value_i = NULL,
                     left_child = NULL,
                     right_child = NULL) {
  node <- new.env()
  node$id <- new_node_id()
  node$indices <- indices
  
  node$prediction <- prediction
  node$split_feature_j <- split_feature_j
  node$split_value_i <- split_value_i
  
  node$left_child <- left_child
  node$right_child <- right_child
  node
}

#=============================================================
# GREEDY CART REGRESSION ALGORITHM
#=============================================================

greedy_cart_regression <- function(input_data, target_variable) {
  
  # variables
  n <- nrow(input_data) # number of data points
  d <- ncol(input_data) # number of features
  
  # starting Tree T^(0)
  root <- new_node(
    indices = 1:n,
    prediction = mean(target_variable)
  )
  
  repeat{
    
    # starting value initialization
    best_risk <- Inf
    best_split <- NULL
    
    # recursive looping over leaves
    search_best_split <- function(node) {
      
      # leave has one data point left
      if (length(node$indices) <= 1) return()
      
      # selecting targeted data and corresponding target variables
      node_indices <- node$indices
      indices_data <- input_data[node_indices, , drop = FALSE]
      indices_target_variable <- target_variable[node_indices]
      
      # iterating over dimensions/ features d
      for (j in 1:d) {
        
        current_feature <- indices_data[, j]
        vals <- sort(current_feature)
        
        # pairwise split point calculation
        split_points <- (vals[-1] + vals[-length(vals)]) / 2
        
        for (s in split_points) {
          
          left_child_node_indices  <- node_indices[current_feature < s]
          right_child_node_indices <- node_indices[current_feature >= s]
          if (length(left_child_node_indices) == 0 || length(right_child_node_indices) == 0) next
          
          # constants calculation
          c1 <- mean(target_variable[left_child_node_indices])
          c2 <- mean(target_variable[right_child_node_indices])
          
          # risk calculation
          risk <- sum((target_variable[left_child_node_indices]  - c1)^2) +
            sum((target_variable[right_child_node_indices] - c2)^2)
          
          # checking if new best split has been found
          if (risk < best_risk) {
            best_risk <<- risk
            best_split <<- list(
              node = node,
              feature = j,
              split = s,
              left_child_node_indices = left_child_node_indices,
              right_child_node_indices = right_child_node_indices,
              c1 = c1,
              c2 = c2
            )
          }
        }
      }
    }
    
    # recursive traversing of leaves
    traverse <- function(node) {
      if (is.null(node$split_feature_j)) {
        search_best_split(node)
        return()
      }
      traverse(node$left_child)
      traverse(node$right_child)
    }
    
    traverse(root)
    
    # no split possible
    if (is.null(best_split)) break
    
    # perform the split
    node <- best_split$node
    node$split_feature_j <- best_split$feature
    node$split_value_i <- best_split$split
    
    node$left_child <- new_node(
      indices = best_split$left_child_node_indices,
      prediction = best_split$c1
    )
    
    node$right_child <- new_node(
      indices = best_split$right_child_node_indices,
      prediction = best_split$c2
    )
  }
  
  return(root)
}


#=============================================================
# GREEDY CART CLASSIFICATION ALGORITHM
#=============================================================

greedy_cart_classification <- function(input_data, target_variable) {
  
  n <- nrow(input_data)
  d <- ncol(input_data)
  
  # classes
  classes <- sort(unique(target_variable))
  
  # starting Tree T^(0)
  root <- new_node(
    indices = 1:n,
    prediction = names(which.max(table(target_variable)))
  )
  
  repeat {
    
    # starting value initialization
    best_risk <- Inf
    best_split <- NULL
    
    # recursive looping over leaves
    search_best_split <- function(node) {
      
      # leave has one data point left
      if (length(node$indices) <= 1) return()
      
      # selecting targeted data and corresponding target variables
      node_indices <- node$indices
      indices_data <- input_data[node_indices, , drop = FALSE]
      indices_target_variable <- target_variable[node_indices]
      
      # iterating over dimensions/ features d
      for (j in 1:d) {
        
        current_feature <- indices_data[, j]
        vals <- sort(current_feature)
        
        # split point calculation
        split_points <- (vals[-1] + vals[-length(vals)]) / 2
        
        for (s in split_points) {
          left_child_node_indices  <- node_indices[current_feature < s]
          right_child_node_indices <- node_indices[current_feature >= s]
          
          if (length(left_child_node_indices) == 0 || length(right_child_node_indices) == 0) next
          
          # determine the class majority in both nodes
          c1 <- names(which.max(table(target_variable[left_child_node_indices])))
          c2 <- names(which.max(table(target_variable[right_child_node_indices])))
          
          # risk calculation
          p1 <- table(target_variable[left_child_node_indices]) / length(left_child_node_indices)
          p2 <- table(target_variable[right_child_node_indices]) / length(right_child_node_indices)
          
          r1 <- length(left_child_node_indices) * (1 - ifelse(c1 %in% names(p1), p1[c1], 0))
          r2 <- length(right_child_node_indices) * (1 - ifelse(c2 %in% names(p2), p2[c2], 0))
          
          risk <- r1 + r2
          
          # checking if new best split has been found
          if (risk < best_risk) {
            best_risk <<- risk
            best_split <<- list(
              node = node,
              feature = j,
              split = s,
              left_child_node_indices = left_child_node_indices,
              right_child_node_indices = right_child_node_indices,
              c1 = c1,
              c2 = c2
            )
          }
        }
      }
    }
    
    # recursive traversing of leaves
    traverse <- function(node) {
      if (is.null(node$split_feature_j)) {
        search_best_split(node)
        return()
      }
      traverse(node$left_child)
      traverse(node$right_child)
    }
    
    traverse(root)
    
    # no split possible
    if (is.null(best_split)) break
    
    # perform the split
    node <- best_split$node
    node$split_feature_j <- best_split$feature
    node$split_value_i <- best_split$split
    
    node$left_child <- new_node(
      indices = best_split$left_child_node_indices,
      prediction = best_split$c1
    )
    
    node$right_child <- new_node(
      indices = best_split$right_child_node_indices,
      prediction = best_split$c2
    )
  }
  
  return(root)
}
