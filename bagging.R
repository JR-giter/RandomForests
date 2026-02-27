bootstrap_sample <- function(dataSet) {
  n <- nrow(dataSet)
  idx <- sample(seq_len(n), size = n, replace = TRUE) # ziehen mit zurücklegen
  dataSet[idx, , drop = FALSE]
}







bagging_greedycart <- function(data, n_bootstrapSamples = 50, n_properties, n_nodes, target) {
  
  models <- vector("list", n_bootstrapSamples)
  
  for (b in seq_len(n_bootstrapSamples)) {
    
    boot <- bootstrap_sample(data)
    
    models[[b]] <- GreedyCart(
      dataSet = boot,
      n_properties = n_properties,
      n_nodes = n_nodes,
      mode = "regression",
      target = target
    )
  }
  
  models
}





#predict_bagging <- function(models, newdata) {
#  preds <- sapply(models, function(m) predict_cart(m, newdata))
#  rowMeans(preds)
#}





# --- TESTLAUF ---
models <- bagging_greedycart(
  data = ames,
  n_bootstrapSamples = 3,                 # klein zum Testen
  n_properties = 3,
  n_nodes = 10,
  target = "Sale_Price"
)

models

plot_cart_tree(models[[1]])
plot_cart_tree(models[[2]])
plot_cart_tree(models[[3]])
