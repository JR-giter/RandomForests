source(Greedy_Cart.R)
source(Plotting_Trees.R)
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
## EXAMPLE G-CART CLASSIFICATION ALGORITHM
############################################################

{
  data <- createClassificationDataExample(10)
  tree_cla <- greedy_cart_classification(data$X, data$Y)
  plot_cart_tree(tree_cla)
}

############################################################
## EXAMPLE G-CART REGRESSION ALGORITHM
############################################################

{
  # input_data <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
  # target_variable <- c(2, 3, 2, 8, 9)
  # tree <- greedy_cart_regression(input_data, target_variable)
  # plot_cart_tree(tree)
  
  data <- createSinDataExample(20)
  input_data <- matrix(data$x, ncol=1)
  target_variable <- matrix(data$y)
  tree_reg <- greedy_cart_regression(input_data, target_variable)
  plot_cart_tree(tree_reg)
}
