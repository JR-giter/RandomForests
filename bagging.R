# generating a boootstrap sample out of our data set
bootstrap_sample <- function(dataSet) {
  n <- nrow(dataSet)
  idx <- sample(seq_len(n), size = n, replace = TRUE) # ziehen mit zurücklegen
  dataSet[idx, , drop = FALSE]
}

# generating "n_bootstrapSamples" out of the first 
bagging_greedycart <- function(data, n_bootstrapSamples, n_properties, n_nodes, target) {
  
  # empty list to store each tree
  models <- vector("list", n_bootstrapSamples)
  
  # fill "models" with bootstrapsamples
  for (b in seq_len(n_bootstrapSamples)) {
    # generate one bootstrap sample
    boot <- bootstrap_sample(data)
    # generate model/ tree out of "boot" 
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


