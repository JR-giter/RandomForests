source("Greedy_Cart.R")
source("Plotting_Trees.R")
source("Pruning.R")
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

# {
#   data <- createClassificationDataExample(10)
#   tree_cla <- greedy_cart_classification(data$X, data$Y)
#   plot_cart_tree(tree_cla)
# }

############################################################
## EXAMPLE G-CART REGRESSION ALGORITHM
############################################################

# {
#   input_data <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
#   target_variable <- c(2, 3, 2, 8, 9)
#   tree <- greedy_cart_regression(input_data, target_variable)
#   plot_cart_tree(tree)
# 
#   data <- createSinDataExample(20)
#   input_data <- matrix(data$x, ncol=1)
#   target_variable <- matrix(data$y)
#   tree_reg <- greedy_cart_regression(input_data, target_variable)
#   plot_cart_tree(tree_reg)
# }

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Prepare Data
actual <- c(215000, 105000, 172000, 244000, 189900, 195500)
predicted <- c(223500, 147000, 158000, 169000, 215000, 215000)
max_val <- max(c(actual, predicted))

data <- data.frame(
  Group = factor(1:6),
  SalePrice = actual,
  Prediction = predicted,
  RelError_Scaled = (abs(actual - predicted) / actual) * max_val,
  RelError_Pct = (abs(actual - predicted) / actual)
)

# 2. Reshape and set Factor levels to control bar order (Grey on right)
data_long <- data %>%
  select(Group, SalePrice, Prediction, RelError_Scaled) %>%
  pivot_longer(cols = -Group, names_to = "Type", values_to = "Value") %>%
  mutate(Type = factor(Type, levels = c("SalePrice", "Prediction", "RelError_Scaled")))

# 3. Plot
ggplot() +
  # Main Bar Layer
  geom_bar(data = data_long, aes(x = Group, y = Value, fill = Type), 
           stat = "identity", position = position_dodge(width = 0.8)) +
  
  # Text for SalePrice (Blue Bar - Left)
  geom_text(data = data, 
            aes(x = Group, y = SalePrice, label = scales::comma(SalePrice)),
            inherit.aes = FALSE, nudge_x = -0.27, vjust = -0.5, size = 2.5) +
  
  # Text for Prediction (Red Bar - Middle)
  geom_text(data = data, 
            aes(x = Group, y = Prediction, label = scales::comma(Prediction)),
            inherit.aes = FALSE, nudge_x = 0, vjust = -0.5, size = 2.5) +
  
  # Text for RelError (Grey Bar - Right)
  geom_text(data = data, 
            aes(x = Group, y = RelError_Scaled, 
                label = paste0(round(RelError_Pct * 100, 1), "%")),
            inherit.aes = FALSE, nudge_x = 0.27, vjust = -0.5, 
            size = 3, color = "black", fontface = "bold") +
  
  scale_fill_manual(values = c(
    "SalePrice" = "#3498db", 
    "Prediction" = "#e74c3c", 
    "RelError_Scaled" = "#95a5a6"
  ), labels = c("SalePrice" = "Actual Price", 
                "Prediction" = "Prediction", 
                "RelError_Scaled" = "Relative Error")) +
  
  scale_y_continuous(
    name = "Price ($)",
    expand = expansion(mult = c(0, 0.15)), # Extra space at top for labels
    sec.axis = sec_axis(~ . / max_val * 100, name = "Relative Error (%)")
  ) +
  labs(
    title = "Comparison: Actual vs. Predicted vs. Error Magnitude",
    x = "House Sample ID",
    fill = "Legend"
  ) +
  theme_minimal()


##### Pruning
data <- createSinDataExample(20)
input_data <- matrix(data$x, ncol=1)
target_variable <- matrix(data$y)
tree <- greedy_cart_regression(input_data, target_variable)
plot_cart_tree(tree)

pruning <- cost_complexity_pruning(tree, target_variable, "regression")

best_tree <- select_tree_lambda(pruning,
                                lambda = 10,
                                target_variable = target_variable,
                                type="regression")
plot_cart_tree(best_tree)
