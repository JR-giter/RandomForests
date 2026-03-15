# ==========================================================
# SOURCES & LIBRARIES
# ==========================================================

library(RandomForests)
library(shiny)
library(shinyjs)

# ==========================================================
# Initial Test DataSet
# ==========================================================

install.packages("AmesHousing")
library(AmesHousing)
ames <- make_ames()

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
                          numericInput("nodes", NULL, 100, min = 5)
                   ),

                   # Properties
                   column(6,
                          div(style = "display:flex; justify-content:space-between;",
                              tags$label("Properties:"),
                              tags$small("Top‑k Features")
                          ),
                          numericInput("properties", NULL, 5, min = 1)
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
                   numericInput("bootstrap", NULL, 20, min = 1),
                   hr()
                 ),

                 conditionalPanel(
                   condition = "input.model_choice == 'Random Forest'",
                   h4("Random Forest Parameters"),
                   div(style = "display:flex; justify-content:space-between;",
                       tags$label("Tree Count:"),
                       tags$small("Anzahl Bäume")
                   ),
                   numericInput("treecount", NULL, 50, min = 1),
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
                 uiOutput("results_block"),

                 fluidRow(
                   column(12, wellPanel(
                     h3("Evaluator"),
                     uiOutput("dynamic_inputs"),
                     hr(),
                     h4("Estimated Value/Class:"),
                     span(textOutput("prediction_output"),
                          style="font-size: 28px; color: #2c3e50; font-weight: bold;")
                   ))
                 )


               ))
             )
    ),

    # ---------------------------------------------------------
    # TAB 3: Tests
    # ---------------------------------------------------------



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




  # Run Model Button
  observeEvent(input$run_model, {

    # ------------------------------------------------------
    # 1. Dataset laden
    # ------------------------------------------------------
    data_df <- get(input$dataset)

    train_data <- data_df[1:input$nodes, ]
    test_data <- data_df[(nrow(data_df) - 29):nrow(data_df), ]


    # Runtime starten
    start_time <- proc.time()

    # ------------------------------------------------------
    # Modell abhängig von der Auswahl erzeugen + testen
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

      if (input$mode == "regression") {

        preds <- predict_rf(res_model, test_data)
        actual_vals <- test_data[[input$target]]

        results <- data.frame(
          actual     = actual_vals,
          prediction = preds,
          delta      = (abs(actual_vals - as.numeric(preds)) / actual_vals) * 100
        )

      } else {

        # Classification → test_rf benutzen
        results <- test_rf(res_model, test_data)

      }

    }

    # save Model
    trained_model(res_model)
    active_features(get_model_features(res_model))


  # ------------------------------------------------------
  # Metrics Block
  # ------------------------------------------------------

    # record runtime end
    runtime <- proc.time() - start_time

    # get metrics depending on mode
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

    # render Results Block
    output$results_block <- renderUI({
      req(trained_model())   # Nur anzeigen, wenn ein Modell existiert

      tagList(
        h4("Results"),
        tags$small("Evaluation auf 30 Testobjekten"),
        br(),
        verbatimTextOutput("runtime"),
        verbatimTextOutput("metrics"),
        hr(),

      )
    })

  })

  # ------------------------------------------------------
  # dynamic inputs based on active features of the model
  # ------------------------------------------------------
  output$dynamic_inputs <- renderUI({
    req(trained_model())
    req(active_features())

    feats <- active_features()
    data_df <- get(input$dataset)

    fluidRow(
      lapply(feats, function(f) {
        avg_val <- if (is.numeric(data_df[[f]]))
          round(mean(data_df[[f]], na.rm = TRUE), 1)
        else 0

        column(3, numericInput(
          inputId = paste0("pred_", f),
          label   = f,
          value   = avg_val,
          min = 0
        ))
      })
    )
  })


  # ------------------------------------------------------
  # Predictions
  # ------------------------------------------------------
  output$prediction_output <- renderText({
    req(trained_model())
    req(active_features())

    model <- trained_model()
    feats <- active_features()

    # Correctly identify properties for the input vector
    props <- if (!is.null(model$trees)) {
      model$trees[[1]]$feature_names
    } else if (is.list(model) && is.environment(model[[1]])) {
      model[[1]]$properties
    } else if (is.environment(model)) {
      model$properties
    } else {
      model$properties
    }

    # Werte aus den Inputs holen
    input_vals <- sapply(props, function(f) {
      val <- input[[paste0("pred_", f)]]
      if (is.null(val)) 0 else val
    })

    res <- NULL

    tryCatch({

      # GREEDY + PRUNING
      if (input$model_choice %in% c("Greedy", "Pruning")) {
        res <- predict_cart(model, input_vals)
      }

      # BAGGING
      else if (input$model_choice == "Bagging") {

        preds <- sapply(model, function(tree) {
          p <- predict_cart(tree, input_vals)
          if (input$mode == "classification") as.character(p) else p
        })

        if (input$mode == "regression") {
          res <- mean(as.numeric(preds))
        } else {
          res <- names(sort(table(preds), decreasing = TRUE))[1]
        }
      }

      # RANDOM FOREST
      else if (input$model_choice == "Random Forest") {

        df_row <- as.data.frame(t(input_vals))
        colnames(df_row) <- props

        res <- predict_rf(model, df_row)
      }


      # FORMATIERUNG
      if (input$mode == "regression") {

        is_money <- grepl("price|cost|value|sale", tolower(input$target))

        if (is_money) {
          return(paste0("$", format(round(res, 2), big.mark=",")))
        } else {
          return(round(res, 2))
        }
      }

      # Classification
      return(as.character(res))

    }, error = function(e) {
      return("Ready…")
    })
  })




}

shinyApp(ui, server)
