# Benchmarking and Runtime tests:
#
# Measurements:
#   - Runtime (how long does it take to run)
#   - Scalability (how does RT scale with differnt inputs)
# TODO   - Edge Cases ? (extreme values, empty inputs, NA handling etc.)

# =============================================================
# Benchmark Automation
# =============================================================

benchmark <- function(dataSet, properties, 
                      n_nodes, test_start_node, 
                      test_end_node, n_runs){
  
  total_start_time <- Sys.time()
  results <- vector("list", n_runs)
  
  pb <- txtProgressBar(min = 0, max = n_runs, style = 3)
  
  for(i in seq_len(n_runs)){
    start_time <- Sys.time()
    
    tree_i <- generate_cart_tree(
      dataSet    = ames,
      properties = properties,
      n_nodes    = n_nodes,
      mode       = "regression",
      target     = "Sale_Price"
    )
    
    end_time <- Sys.time()
    
    test_i <- test_cart( tree = tree_i,
                         dataPoints = ames[test_start_node:test_end_node,], 
                         mode = "regression",
                         target = "Sale_Price")
    
    c_test <- calc_results(test_i)
    
    results[[i]] <- list(
      max = c_test[["max"]],
      min = c_test[["min"]],
      mean = c_test[["mean"]],
      median = c_test[["median"]],
      runtime = as.numeric(end_time - start_time, units = "secs")
    )
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  total_end_time <- Sys.time()
  total_runtime <- as.numeric(total_end_time - total_start_time, units = "secs")
  
  if (total_runtime < 60) {
    cat("Total Runtime:", round(total_runtime, 2), "sec\n")
  } else {
    cat("Total Runtime:", round(total_runtime / 60, 2), "min\n")
  }
  
  
  df <- do.call(rbind, lapply(results, as.data.frame))
  df
}

run_and_plot_benchmarks <- function(
    title,
    x_name,
    dataSet,
    properties,
    n_nodes,
    test_start_node,
    test_end_node,
    n_runs
){
  # --- 1) Erkennen, welcher Parameter ein Vektor ist ---
  param_list <- list(
    properties      = properties,
    n_nodes         = n_nodes,
    test_start_node = test_start_node
  )
  
  # Der Vektor ist der Parameter mit Länge > 1
  vector_param <- names(param_list)[which(sapply(param_list, length) > 1)]
  vector_values <- param_list[[vector_param]]
  
  if (length(vector_param) != 1) {
    stop("Exactly one of: properties, n_nodes, test_start_node must be a vector.")
  }
  
  # --- 2) Benchmarks ausführen ---
  benchmarks <- lapply(vector_values, function(v){
    benchmark(
      dataSet = dataSet,
      properties = if (vector_param == "properties") v else properties,
      n_nodes = if (vector_param == "n_nodes") v else n_nodes,
      test_start_node = if (vector_param == "test_start_node") v else test_start_node,
      test_end_node = test_end_node,
      n_runs = n_runs
    )
  })
  
  # --- 3) Letzte Werte extrahieren ---
  extract_last <- function(df) {
    tail(df, 1)[, c("mean", "median", "runtime")]
  }
  
  bm_last <- do.call(rbind, lapply(seq_along(benchmarks), function(i){
    cbind(
      x_index = i,
      x_value = vector_values[i],
      extract_last(benchmarks[[i]])
    )
  }))
  
  # --- 4) Long Format für ggplot ---
  library(tidyr)
  library(ggplot2)
  
  plot_data <- pivot_longer(
    bm_last,
    cols = c(mean, median, runtime),
    names_to = "metric",
    values_to = "value"
  )
  
  # --- 5) Plot ---
  ggplot(plot_data, aes(x = x_index, y = value, color = metric, group = metric)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(
      breaks = plot_data$x_index,
      labels = plot_data$x_value
    ) +
    labs(
      title = title,
      x = x_name,
      y = "Value",
      color = "Metric"
    ) +
    theme_minimal(base_size = 14)
}



# =============================================================
# Benchmarking for Property Number
# =============================================================
run_and_plot_benchmarks(
  title = "Benchmarking Properties",
  x_name = "Number of Properties",
  dataSet = ames,
  properties = c(5, 10, 20, 30),
  n_nodes = 500,
  test_start_node = 2000,
  test_end_node = 2500,
  n_runs = 10
)

# =============================================================
# Benchmarking for Number of Nodes
# =============================================================
run_and_plot_benchmarks(
  title = "Benchmarking Number of Nodes",
  x_name = "Number of Nodes",
  dataSet = ames,
  properties = 10,
  n_nodes = c(250, 500, 1000, 2000),
  test_start_node = 2000,
  test_end_node = 2500,
  n_runs = 10
)

# =============================================================
# Benchmarking for Number of Test Nodes
# =============================================================
run_and_plot_benchmarks(
  title = "Benchmarking Number of Test Nodes",
  x_name = "Number of Test Nodes",
  dataSet = ames,
  properties = 10,
  n_nodes = 500,
  test_start_node = c(500, 1000, 1500, 2000),
  test_end_node = 2500,
  n_runs = 10
)
