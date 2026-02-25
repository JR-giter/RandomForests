#############################################################
## BASIC-NODE STRUCTURE
#############################################################

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

############################################################
## GREEDY CART REGRESSION ALGORITHM
############################################################

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


############################################################
## GREEDY CART CLASSIFICATION ALGORITHM
############################################################

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
