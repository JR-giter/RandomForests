# visualizing results
show_results <- function(test_results){
  # comparing test results to actual results
  # print(test_results)

  print(calc_results(test_results))

  # plotting results

  hist(test_results$delta,
       breaks = 40,
       main = "Distribution of Percentage Delta",
       xlab = "Delta (%)",
       col = "steelblue",
       border = "white")

  plot(density(test_results$delta),
       main = "Density of Percentage Delta",
       xlab = "Delta (%)",
       lwd = 2)
}

calc_results <- function(test_results){
  return(
    c(
      max = max(test_results$delta, na.rm = TRUE),
      min = min (test_results$delta, na.rm = TRUE),
      mean = mean(test_results$delta, na.rm = TRUE),
      median = median(test_results$delta, na.rm = TRUE)
    )
  )
}

# =============================================================
# Plotting Trees
# =============================================================



# Initialize a counter in a separate environment (mutable)
.node_env <- new.env(parent = emptyenv())
.node_env$.node_id_counter <- 0L

new_node_id <- function() {
  # Increment the counter inside the environment
  .node_env$.node_id_counter <- .node_env$.node_id_counter + 1L
  paste0("node_", .node_env$.node_id_counter)
}

tree_to_df <- function(node) {

  leaf_index <- 0
  nodes <- data.frame(
    id = character(),
    x = numeric(),
    y = numeric(),
    label = character(),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = character(),
    to   = character(),
    stringsAsFactors = FALSE
  )

  layout_tree <- function(node, depth = 0) {

    is_leaf <- is.null(node$split_feature_j)

    label <- if (is_leaf) {
      pred <- node$prediction
      paste0("Leaf\nn=", length(node$indices),
             if (is.numeric(pred)) {
               paste0("\nŷ=", round(pred, 2))
             } else {
               paste0("\nClass=", pred)
             })
    } else {
      paste0("x", node$split_feature_j,
             " < ", round(node$split_value_i, 2))
    }

    # ---- LEAF ----
    if (is_leaf) {
      leaf_index <<- leaf_index + 1
      x_pos <- leaf_index

      nodes <<- rbind(nodes, data.frame(
        id = node$id,
        x = x_pos,
        y = -depth,
        label = label
      ))

      return(x_pos)
    }

    # ---- INTERNAL NODE ----
    left_x  <- layout_tree(node$left_child, depth + 1)
    right_x <- layout_tree(node$right_child, depth + 1)

    x_pos <- (left_x + right_x) / 2

    nodes <<- rbind(nodes, data.frame(
      id = node$id,
      x = x_pos,
      y = -depth,
      label = label
    ))

    edges <<- rbind(edges,
                    data.frame(from=node$id,to=node$left_child$id),
                    data.frame(from=node$id,to=node$right_child$id))

    return(x_pos)
  }

  layout_tree(node)

  list(nodes = nodes, edges = edges)
}


plot_cart_tree <- function(tree) {
  td <- tree_to_df(tree)

  edges <- merge(td$edges, td$nodes, by.x = "from", by.y = "id")
  edges <- merge(edges, td$nodes, by.x = "to", by.y = "id",
                 suffixes = c("_p", "_c"))

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
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = 0.2))
}


