library(ggplot2)
############################################################
## PLOTTING WITH GGPLOT2
############################################################

{
    .node_id_counter <- 0
    new_node_id <- function() {
        .node_id_counter <<- .node_id_counter + 1
        paste0("node_", .node_id_counter)
    }
    
    tree_to_df <- function(node, depth = 0, x = 0, dx = 1) {
        
        # check if is leaf
        is_leaf <- is.null(node$split_feature_j)
        
        label <- if (is_leaf) {
            # leaf: regression value or classification prediction
            pred <- node$prediction
            paste0("Leaf\nn=", length(node$indices),
                   if (is.numeric(pred)) {
                       paste0("\nŷ=", round(pred, 2))
                   } else {
                       paste0("\nClass=", pred)
                   })
        } else {
            # inner Node
            paste0("x", node$split_feature_j,
                   " < ", round(node$split_value_i, 2))
        }
        
        nodes <- data.frame(
            id = node$id,
            x = x,
            y = -depth,
            label = label,
            stringsAsFactors = FALSE
        )
        
        edges <- data.frame(
            from = character(),
            to   = character(),
            stringsAsFactors = FALSE
        )
        
        # recursive traversal
        if (!is.null(node$left_child)) {
            left_child <- tree_to_df(node$left_child, depth + 1, x - dx, dx / 2)
            edges <- rbind(edges, data.frame(from = node$id, to = node$left_child$id))
            nodes <- rbind(nodes, left_child$nodes)
            edges <- rbind(edges, left_child$edges)
        }
        
        if (!is.null(node$right_child)) {
            right_child <- tree_to_df(node$right_child, depth + 1, x + dx, dx / 2)
            edges <- rbind(edges, data.frame(from = node$id, to = node$right_child$id))
            nodes <- rbind(nodes, right_child$nodes)
            edges <- rbind(edges, right_child$edges)
        }
        
        list(nodes = nodes, edges = edges)
    }
    
    plot_cart_tree <- function(tree) {
        td <- tree_to_df(tree)
        
        edges <- merge(td$edges, td$nodes, by.x = "from", by.y = "id")
        edges <- merge(edges, td$nodes, by.x = "to", by.y = "id",
                       suffixes = c("_p", "_c"))
        
        library(ggplot2)
        
        ggplot() +
            geom_segment(
                data = edges,
                aes(x = x_p, y = y_p, xend = x_c, yend = y_c)
            ) +
            geom_label(
                data = td$nodes,
                aes(x = x, y = y, label = label),
                size = 3,
                label.size = 0.25
            ) +
            theme_void() +
            coord_fixed() +
            expand_limits(y = min(td$nodes$y) - 1)
    }
}

############################################################
## BASIC-NODE STRUCTURE
############################################################

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
## EXAMPLE DATA FOR REGRESSION
############################################################

createSinDataExample <- function(n, sigma = 0.2){
    x <- runif(n, 0, 1)                   # X ~ U[0,1]
    eps <- rnorm(n, mean = 0, sd = sigma) # ε ~ N(0, σ²)
    y <- sin(2 * pi * x) + eps            # model
    
    plot(x, y, pch = 5)
    return(data.frame(x = x, y = y))
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
## EXAMPLE G-CART REGRESSION ALGORITHM
############################################################

{
    input_data <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
    target_variable <- c(2, 3, 2, 8, 9)
    tree <- greedy_cart_regression(input_data, target_variable)
    plot_cart_tree(tree)
    
    data <- createSinDataExample(20)
    input_data <- matrix(data$x, ncol=1)
    target_variable <- matrix(data$y)
    tree_reg <- greedy_cart_regression(input_data, target_variable)
    plot_cart_tree(tree_reg)
}












############################################################
## EXAMPLE DATA FOR CLASSIFICATION 
############################################################

createClassificationDataExample <- function(n, sigma = 0.2){
  x1 <- runif(n, 0, 1)
  x2 <- runif(n, 0, 1)
  
  eps <- rnorm(n, mean = 0, sd = sigma)
  
  kappa <- x2 - 0.5 - 0.3 * sin(2 * pi * x1)
  
  y <- ifelse(kappa - eps <= 0, 1, 2)
  
  plot(x1, x2,
       col = ifelse(y == 1, "red", "blue"),
       pch = 16, xlab = "x1", ylab = "x2")
  legend("topright", legend = c("Class 1", "Class 2"),
         col = c("red", "blue"), pch = 16)
  
  list(
    X = data.frame(x1 = x1, x2 = x2),
    Y = y
  )
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

############################################################
## EXAMPLE G-CART CLASSIFICATION ALGORITHM
############################################################

{
  data <- createClassificationDataExample(10)
  tree_cla <- greedy_cart_classification(data$X, data$Y)
  plot_cart_tree(tree_cla)
}

