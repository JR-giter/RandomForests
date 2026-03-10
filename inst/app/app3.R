# source("globals.R")
# source("Greedy_Cart.R")
# source("Plotting.R")
# source("RandomForrest.R")
# source("Bagging.R")
# source("Pruning.R")
# source("Preprocessing.R")
library(RandomForests)
library(shiny)
library(shinyjs)

# ==========================================================
# UNIVERSAL FEATURE EXTRACTOR
# ==========================================================
get_model_features <- function(model_obj) {
  get_names <- function(node) {
    if (!is.null(node$feature_names)) return(node$feature_names)
    if (!is.null(node$properties)) return(node$properties)
    return(NULL)
  }

  crawl_tree <- function(node, master_names) {
    current_dict <- get_names(node)
    if (!is.null(current_dict)) master_names <- current_dict
    if (is.null(node) || is.null(node$split_feature_j)) return(character(0))
    if (is.null(master_names)) return(character(0))

    current_feat <- master_names[node$split_feature_j]
    left  <- crawl_tree(node$left_child, master_names)
    right <- crawl_tree(node$right_child, master_names)
    return(c(current_feat, left, right))
  }

  all_found <- NULL
  if (is.list(model_obj) && !is.null(model_obj$trees)) {
    initial_names <- get_names(model_obj$trees[[1]])
    all_found <- unlist(lapply(model_obj$trees, crawl_tree, master_names = initial_names))
  } else if (is.list(model_obj) && !is.null(model_obj$optimal_tree)) {
    all_found <- crawl_tree(model_obj$optimal_tree, get_names(model_obj$optimal_tree))
  } else if (is.list(model_obj) && length(model_obj) > 0 && is.environment(model_obj[[1]])) {
    all_found <- unlist(lapply(model_obj, function(t) crawl_tree(t, get_names(t))))
  } else if (is.environment(model_obj)) {
    all_found <- crawl_tree(model_obj, get_names(model_obj))
  }

  result <- sort(unique(as.character(all_found[!is.na(all_found) & all_found != ""])))
  message("Features found: ", length(result))
  return(result)
}

# ==========================================================
# SHINY UI
# ==========================================================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Advanced CART Framework"),

  fluidRow(
    column(12, wellPanel(
      h3("Tree Visualization"),
      sliderInput("slider", "Initial Nodes:", min = 1, max = 200, value = 50),
      plotOutput("tree_plot", height = "400px"),
      actionButton("prune_button", "Prune This Tree", class = "btn-warning")
    ))
  ),

  fluidRow(
    column(12, wellPanel(
      h3("Model Configuration"),
      fluidRow(
        column(3, radioButtons("model_choice", "Select Model:",
                               choices = c("Greedy", "Pruning", "Bagging", "Random Forest"))),
        column(3, textInput("dataset", "Dataset:", value = "ames"),
               selectInput("mode", "Mode:", choices = c("regression", "classification")),
               textInput("target", "Target:", value = "Sale_Price")),
        column(3, numericInput("nodes", "Nodes:", 100),
               numericInput("properties", "Properties:", 5)),
        column(3, numericInput("bootstrap", "Bootstrap Samples:", 20),
               numericInput("treecount", "Tree Count:", 50))
      ),
      actionButton("run_model", "Train Model", class = "btn-primary", width = "100%"),
      hr(),
      h4("Results (Holdout 2900:2930)"),
      verbatimTextOutput("runtime"),
      verbatimTextOutput("metrics")
    ))
  ),

  fluidRow(
    column(12, wellPanel(
      h3("House Evaluator (Live Prediction)"),
      uiOutput("dynamic_inputs"),
      hr(),
      h4("Estimated Value:"),
      span(textOutput("prediction_output"), style="font-size: 28px; color: #2c3e50; font-weight: bold;")
    ))
  )
)

# ==========================================================
# SHINY SERVER
# ==========================================================
server <- function(input, output, session) {

  preview_tree_obj <- reactiveVal(NULL)
  trained_model <- reactiveVal(NULL)
  active_features <- reactiveVal(NULL)

  # --- 1. PREVIEW LOGIC (FIXED) ---
  # Generate base tree when slider moves
  observeEvent(input$slider, {
    tree <- generate_cart_tree(ames, 5, input$slider, "regression", "Sale_Price")

    preview_tree_obj(tree)
  })

  # Prune the existing preview tree
  observeEvent(input$prune_button, {
    req(preview_tree_obj())
    pruned_res <- prune_tree(preview_tree_obj(), K = 5)
    preview_tree_obj(pruned_res$optimal_tree)
  })

  output$tree_plot <- renderPlot({
    req(preview_tree_obj())
    plot_cart_tree(preview_tree_obj())
  })

  # --- 2. UI TOGGLES ---
  observe({
    toggleElement("bootstrap", condition = (input$model_choice == "Bagging"))
    toggleElement("treecount", condition = (input$model_choice == "Random Forest"))
  })

  # --- 2. RUN MODEL ---
  observeEvent(input$run_model, {
    data_df <- get(input$dataset)
    test_data <- data_df[2900:2930, ]
    res_model <- NULL
    results <- NULL

    runtime <- system.time({
      if (input$model_choice == "Greedy") {
        res_model <- generate_cart_tree(data_df, input$properties, input$nodes, input$mode, input$target)
        results <- test_cart(res_model, test_data, input$mode, input$target)
      } else if (input$model_choice == "Pruning") {
        full <- generate_cart_tree(data_df, input$properties, input$nodes, input$mode, input$target)
        pruned_res <- prune_tree(full, K = 5)
        res_model <- pruned_res$optimal_tree
        results <- test_cart(res_model, test_data, input$mode, input$target)
      } else if (input$model_choice == "Bagging") {
        res_model <- bagging_greedycart(data_df, input$bootstrap, input$properties, input$nodes, input$mode, input$target)
        results <- test_bagging(res_model, test_data, input$mode, input$target)
      }  else if (input$model_choice == "Random Forest") {
        train_subset <- data_df[1:input$nodes, ]
        res_model <- random_forest(
          data   = train_subset,
          target = input$target,
          B      = input$treecount,
          m      = input$properties,
          mode   = input$mode
        )
        preds <- predict_rf(res_model, test_data)
        actual_vals <- test_data[[input$target]]
        results <- data.frame(
          actual     = actual_vals,
          prediction = preds,
          delta      = (abs(actual_vals - as.numeric(preds)) / actual_vals) * 100
        )
      }
    })

    trained_model(res_model)
    active_features(get_model_features(res_model))

    output$runtime <- renderText({ paste("Runtime:", round(runtime["elapsed"], 4), "sec") })
    output$metrics <- renderPrint({
      if(input$mode == "regression") {
        cat("Mean Delta %:   ", round(mean(results$delta, na.rm=T), 4), "\n")
        cat("Median Delta %: ", round(median(results$delta, na.rm=T), 4), "\n")
        cat("Max Delta %:    ", round(max(results$delta, na.rm=T), 4))
      } else {
        cat("Accuracy: ", attr(results, "accuracy"))
      }
    })
  })

  # --- 3. DYNAMIC INPUTS ---
  output$dynamic_inputs <- renderUI({
    req(active_features())
    feats <- active_features()
    data_df <- get(input$dataset)

    fluidRow(
      lapply(feats, function(f) {
        avg_val <- if(is.numeric(data_df[[f]])) round(mean(data_df[[f]], na.rm=T), 1) else 0
        column(3, numericInput(paste0("pred_", f), label = f, value = avg_val))
      })
    )
  })

  # --- 3. PREDICTION ENGINE ---
  output$prediction_output <- renderText({
    req(trained_model(), active_features())
    model <- trained_model()

    # Correctly identify properties for the input vector
    props <- if(!is.null(model$trees)) {
      model$trees[[1]]$feature_names
    } else if(is.environment(model)) {
      model$properties
    } else {
      model$properties
    }

    input_vals <- sapply(props, function(p) {
      val <- input[[paste0("pred_", p)]]
      if(is.null(val)) return(0) else return(val)
    })

    tryCatch({
      if (input$model_choice %in% c("Greedy", "Pruning")) {
        res <- predict_cart(model, input_vals)
      } else if (input$model_choice == "Bagging") {
        res <- mean(sapply(model, function(t) predict_cart(t, input_vals)))
      } else if (input$model_choice == "Random Forest") {
        df_row <- as.data.frame(t(input_vals))
        colnames(df_row) <- props
        res <- predict_rf(model, df_row)
      }

      if(input$mode == "regression") {
        paste0("$", format(round(res, 2), big.mark=","))
      } else {
        as.character(res)
      }
    }, error = function(e) "Ready...")
  })
}

shinyApp(ui, server)

