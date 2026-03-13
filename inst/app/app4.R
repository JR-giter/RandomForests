# ==========================================================
# SOURCES & LIBRARIES
# ==========================================================

library(RandomForests)
library(shiny)
library(shinyjs)


# ==========================================================
# UITILITY FUNCTION
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
  return(result)
}



# ==========================================================
# UI
# ==========================================================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("CART Framework"),

  tabsetPanel(

    # ---------------------------------------------------------
    # TAB 1: PREVIEW
    # ---------------------------------------------------------
    tabPanel("Preview",

             fluidRow(
               column(12, wellPanel(
                 h3("Tree Visualization"),
                 sliderInput("preview_slider", "Initial Nodes:", min = 1, max = 200, value = 20),
                 plotOutput("preview_tree_plot", height = "550px"),
                 actionButton("preview_prune_button", "Prune Tree", class = "btn-primary btn-lg", width = "100%")
               ))
             )
    ),



    # ---------------------------------------------------------
    # TAB 2: MODEL
    # ---------------------------------------------------------
    tabPanel("Model",

             fluidRow(
               column(12, wellPanel(

                 # ------------------------------------------------------
                 # TITLE
                 # ------------------------------------------------------
                 div(style = "text-align:center; margin-bottom:10px;",
                     h3("Model Configuration")
                 ),

                 # ------------------------------------------------------
                 # GENERAL SETTINGS
                 # ------------------------------------------------------
                 h4("General Settings"),

                 fluidRow(

                   # --- Model Choice ---
                   column(4,
                          radioButtons("model_choice", "Select Model:",
                                       choices = c("Greedy", "Pruning", "Bagging", "Random Forest"))
                   ),

                   # --- Dataset + Target ---
                   column(4,

                          # Dataset
                          div(style = "display:flex; justify-content:space-between;",
                              tags$label("Dataset:"),
                              tags$small("Name eines DataSets")
                          ),
                          textInput("dataset", NULL, value = "ames"),

                          # Target
                          div(style = "display:flex; justify-content:space-between; margin-top:10px;",
                              tags$label("Target Variable:"),
                              tags$small("Zielspalte")
                          ),
                          textInput("target", NULL, value = "Sale_Price")
                   ),

                   # --- Mode ---
                   column(4,
                          div(style = "display:flex; justify-content:space-between;",
                              tags$label("Mode:"),
                              tags$small("Regression/Klassifikation")
                          ),
                          selectInput("mode", NULL, choices = c("regression", "classification"))
                   )
                 ),

                 hr(),

                 # ------------------------------------------------------
                 # TREE PARAMETERS
                 # ------------------------------------------------------
                 h4("Tree Parameters"),

                 fluidRow(
                   # Training Samples
                   column(6,
                          div(style = "display:flex; justify-content:space-between;",
                              tags$label("Training Samples:"),
                              tags$small("Zeilen für Baumaufbau")
                          ),
                          numericInput("nodes", NULL, 100)
                   ),

                   # Properties
                   column(6,
                          div(style = "display:flex; justify-content:space-between;",
                              tags$label("Properties:"),
                              tags$small("Top‑k Features")
                          ),
                          numericInput("properties", NULL, 5)
                   )
                 ),

                 hr(),

                 # ------------------------------------------------------
                 # ENSEMBLE PARAMETERS (only visible when needed)
                 # ------------------------------------------------------
                 conditionalPanel(
                   condition = "input.model_choice == 'Bagging'",
                   h4("Bagging Parameters"),
                   div(style = "display:flex; justify-content:space-between;",
                       tags$label("Bootstrap Samples:"),
                       tags$small("Anzahl Bootstrap‑Sets")
                   ),
                   numericInput("bootstrap", NULL, 20),
                   hr()
                 ),

                 conditionalPanel(
                   condition = "input.model_choice == 'Random Forest'",
                   h4("Random Forest Parameters"),
                   div(style = "display:flex; justify-content:space-between;",
                       tags$label("Tree Count:"),
                       tags$small("Anzahl Bäume")
                   ),
                   numericInput("treecount", NULL, 50),
                   hr()
                 ),

                 # ------------------------------------------------------
                 # RUN BUTTON
                 # ------------------------------------------------------
                 actionButton("run_model", "Generate Model",
                              class = "btn-primary btn-lg", width = "100%"),

                 br(),

                 # ------------------------------------------------------
                 # RESULTS
                 # ------------------------------------------------------
                 uiOutput("results_block")

               ))
             )
    ),

    # ---------------------------------------------------------
    # TAB 3: EVALUATOR
    # ---------------------------------------------------------
    tabPanel("Evaluator",
             sidebarLayout(
               sidebarPanel(
                 h3("Input Features"),
                 uiOutput("dynamic_inputs"),
                 actionButton("predict_button", "Predict", class = "btn-success")
               ),
               mainPanel(
                 h3("Prediction"),
                 span(textOutput("prediction_output"),
                      style="font-size: 28px; color: #2c3e50; font-weight: bold;")
               )
             )
    )
  )
)

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session) {

  # ==========================================================
  # PREVIEW TAB
  # ==========================================================

  preview_tree_obj <- reactiveVal(NULL)

  # Baum generieren, wenn Slider bewegt wird
  observeEvent(input$preview_slider, {
    tree <- generate_cart_tree(
      ames,
      5,
      input$preview_slider,
      "regression",
      "Sale_Price"
    )
    preview_tree_obj(tree)
  })

  # Baum prunen
  observeEvent(input$preview_prune_button, {
    req(preview_tree_obj())
    pruned_res <- prune_tree(preview_tree_obj(), K = 5)
    preview_tree_obj(pruned_res$optimal_tree)
  })

  # Baum plotten
  output$preview_tree_plot <- renderPlot({
    req(preview_tree_obj())
    plot_cart_tree(preview_tree_obj())
  })


  # ==========================================================
  # MODEL TAB
  # ==========================================================

  # reactiveVal für späteres Modell
  trained_model <- reactiveVal(NULL)

  # Initial leerer Result Block
  output$results_block <- renderUI({ NULL })

  # Initial NULL für aktive Features
  active_features <- reactiveVal(NULL)

  # Dynamische Inputs für manuelle Prediction basierend auf den verwendeten Features des Modells
  output$dynamic_inputs <- renderUI({
    req(active_features())
    feats <- active_features()

    fluidRow(
      lapply(feats, function(f) {
        column(3, numericInput(
          inputId = paste0("pred_", f),
          label   = f,
          value   = NULL
        ))
      })
    )
  })



  # Run Model Button
  observeEvent(input$run_model, {

    # ------------------------------------------------------
    # 1. Dataset laden
    # ------------------------------------------------------
    data_df <- get(input$dataset)

    train_data <- data_df[1:input$nodes, ]
    test_data  <- data_df[2900:2930, ]

    # ------------------------------------------------------
    # 2. Runtime starten
    # ------------------------------------------------------
    start_time <- proc.time()

    # ------------------------------------------------------
    # 3. Modell abhängig von der Auswahl erzeugen + testen
    # ------------------------------------------------------
    res_model <- NULL
    results   <- NULL

    if (input$model_choice == "Greedy") {

      res_model <- generate_cart_tree(
        data_df,
        input$properties,
        input$nodes,
        input$mode,
        input$target
      )

      results <- test_cart(res_model, test_data, input$mode, input$target)
    }

    else if (input$model_choice == "Pruning") {

      full <- generate_cart_tree(
        data_df,
        input$properties,
        input$nodes,
        input$mode,
        input$target
      )

      pruned_res <- prune_tree(full, K = 5, mode = input$mode)
      res_model  <- pruned_res$optimal_tree

      results <- test_cart(res_model, test_data, input$mode, input$target)
    }

    else if (input$model_choice == "Bagging") {

      res_model <- bagging_greedycart(
        data_df,
        input$bootstrap,
        input$properties,
        input$nodes,
        input$mode,
        input$target
      )

      results <- test_bagging(res_model, test_data, input$mode, input$target)
    }

    else if (input$model_choice == "Random Forest") {

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

    # ------------------------------------------------------
    # 4. Modell speichern
    # ------------------------------------------------------
    trained_model(res_model)
    active_features(get_model_features(res_model))

    # ------------------------------------------------------
    # 5. Runtime stoppen
    # ------------------------------------------------------
    runtime <- proc.time() - start_time

    # ------------------------------------------------------
    # 6. Outputs anzeigen
    # ------------------------------------------------------
    output$runtime <- renderText({
      paste("Runtime:", round(runtime["elapsed"], 4), "sec")
    })

    output$metrics <- renderPrint({
      if (input$mode == "regression") {
        cat("Average Error %:   ", round(mean(results$delta, na.rm = TRUE), 4), "\n")
        cat("Median Error %: ", round(median(results$delta, na.rm = TRUE), 4), "\n")
        cat("Maximum Error %:    ", round(max(results$delta, na.rm = TRUE), 4))
      } else {
        cat("Accuracy: ", attr(results, "accuracy"))
      }
    })

    # ------------------------------------------------------
    # 7. Results-Block sichtbar machen
    # ------------------------------------------------------
    output$results_block <- renderUI({
      req(trained_model())   # Nur anzeigen, wenn ein Modell existiert

      tagList(
        h4("Results"),
        tags$small("Evaluation auf 30 Testobjekten"),
        br(),
        verbatimTextOutput("runtime"),
        verbatimTextOutput("metrics"),
        hr(),

        # ------------------------------------------------------
        # MANUAL PREDICTION SECTION
        # ------------------------------------------------------
        h4("Manual Prediction"),
        tags$small("Geben Sie Werte für die verwendeten Features ein"),
        br(),

        # WICHTIG: dynamic_inputs HIER direkt einfügen
        uiOutput("dynamic_inputs"),

        br(),
        actionButton("predict_button", "Predict",
                     class = "btn-success", width = "100%"),

        br(), br(),

        h4("Prediction Result"),
        textOutput("prediction_output")
      )
    })

  })

}

shinyApp(ui, server)
