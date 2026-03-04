source("globals.R")
source("Greedy_Cart.R")
source("Plotting.R")
source("RandomForrest.R")
source("Bagging.R")
source("Pruning.R")
source("Preprocessing.R")
library(shiny)
library(shinyjs)

ui <- fluidPage(
  
  useShinyjs(),
  titlePanel("CART Framework"),
  
  # ==========================================================
  # 1. TREE VISUALIZATION (UNDEPENDENT)
  # ==========================================================
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("Tree Visualization"),
        
        sliderInput("slider", "Number of Nodes:", 
                    min = 1, max = 200, value = 50),
        
        plotOutput("tree_plot", height = "400px"),
        
        actionButton("prune_button", "Prune")
      )
    )
  ),
  
  # ==========================================================
  # 2. MODEL + HYPERPARAMETER SECTION
  # ==========================================================
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("Model Configuration"),
        
        radioButtons("model_choice", "Select Model:",
                     choices = c("Greedy", 
                                 "Pruning", 
                                 "Bagging", 
                                 "Random Forest")),
        
        numericInput("nodes", "Nodes:", 100),
        numericInput("properties", "Properties:", 5),
        numericInput("bootstrap", "Bootstrap Samples:", 20),
        numericInput("treecount", "Tree Count:", 50),
        
        textInput("dataset", "Dataset:", value = "ames"),
        selectInput("mode", "Mode:", choices = c("regression", "classification")),
        textInput("target", "Target:", value = "Sale_Price"),
        
        actionButton("run_model", "Run"),
        
        hr(),
        h4("Results"),
        verbatimTextOutput("runtime"),
        verbatimTextOutput("max_delta"),
        verbatimTextOutput("mean_delta"),
        verbatimTextOutput("median_delta")
      )
    )
  ),
  
  # ==========================================================
  # 3. PREDICTION SECTION (UNDEPENDENT)
  # ==========================================================
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("Prediction Input"),
        
        numericInput("input1", "Feature 1:", 0),
        numericInput("input2", "Feature 2:", 0),
        numericInput("input3", "Feature 3:", 0),
        numericInput("input4", "Feature 4:", 0),
        numericInput("input5", "Feature 5:", 0),
        
        h4("Estimated Value/Class"),
        textOutput("prediction_output")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ==========================================================
  # 1. TREE VISUALIZATION (UNDEPENDENT)
  # ==========================================================
  
  cart_tree_reactive <- reactive({
    generate_cart_tree(
      dataSet   = ames,
      properties = 5,
      n_nodes    = input$slider,
      mode       = "regression",
      target     = "Sale_Price"
    )
  })
  
  current_tree <- reactiveVal(NULL)
  
  observeEvent(cart_tree_reactive(), {
    current_tree(cart_tree_reactive())
  })
  
  observeEvent(input$prune_button, {
    req(current_tree())
    pruned_tree <- prune_tree(current_tree(), K = 5)
    current_tree(pruned_tree$optimal_tree)
  })
  
  output$tree_plot <- renderPlot({
    req(current_tree())
    plot_cart_tree(current_tree())
  })
  
  
  # ==========================================================
  # 2. MODEL EXECUTION + PERFORMANCE
  # ==========================================================
  observe({
    
    # Erst alles deaktivieren
    disable("bootstrap")
    disable("treecount")
    
    model <- input$model_choice
    
    if (model == "Greedy") {
      # Nur Standardfelder
      # nichts extra aktivieren
    }
    
    if (model == "Pruning") {
      # Gleich wie Greedy
    }
    
    if (model == "Bagging") {
      enable("bootstrap")
    }
    
    if (model == "Random Forest") {
      enable("treecount")
    }
  })
  
  
  observeEvent(input$run_model, {
    
    model <- input$model_choice
    test_results <- NULL
    
    runtime <- system.time({
      
      if (model == "Greedy") {
        
        cart_tree <- generate_cart_tree(
          dataSet = ames,
          properties = input$properties,
          n_nodes = input$nodes,
          mode = "regression",
          target = "Sale_Price"
        )
        
        test_results <- test_cart(
          tree = cart_tree,
          dataPoints = ames[2900:2930,],
          mode = "regression",
          target = "Sale_Price"
        )
      }
      
      if (model == "Pruning") {
        
        cart_tree <- generate_cart_tree(
          dataSet = ames,
          properties = input$properties,
          n_nodes = input$nodes,
          mode = "regression",
          target = "Sale_Price"
        )
        
        pruned_tree <- prune_tree(cart_tree, K = 5)
        
        test_results <- test_cart(
          tree = pruned_tree$optimal_tree,
          dataPoints = ames[2900:2930,],
          mode = "regression",
          target = "Sale_Price"
        )
      }
      
      if (model == "Bagging") {
        
        models <- bagging_greedycart(
          data = ames,
          n_bootstrapSamples = input$bootstrap,
          properties = input$properties,
          n_nodes = input$nodes,
          target = "Sale_Price"
        )
        
        test_results <- test_bagging(
          models = models,
          dataPoints = ames[2900:2930,],
          mode = "regression",
          target = "Sale_Price"
        )
      }
      
      if (model == "Random Forest") {
        
        train_data <- ames[1:100, ]
        test_data  <- ames[2900:2930, ]
        
        my_rf <- random_forest(
          data = train_data,
          B = input$treecount,
          m = input$properties,
          target = "Sale_Price",
          mode = "regression"
        )
        
        predictions <- predict_rf(my_rf, test_data)
        
        test_results <- data.frame(
          Actual = test_data$Sale_Price,
          Predicted = predictions,
          Error = abs(test_data$Sale_Price - predictions),
          delta  = (abs(test_data$Sale_Price - predictions) / 
                      test_data$Sale_Price) * 100
        )
      }
      
    })
    
    # Performance metrics
    max_delta <- max(test_results$delta, na.rm = TRUE)
    mean_delta <- mean(test_results$delta, na.rm = TRUE)
    median_delta <- median(test_results$delta, na.rm = TRUE)
    
    output$runtime <- renderText({
      paste("System Time (sec):", round(runtime["elapsed"], 4))
    })
    
    output$max_delta <- renderText({
      paste("Max % Missprediction:", round(max_delta, 4))
    })
    
    output$mean_delta <- renderText({
      paste("Mean % Missprediction:", round(mean_delta, 4))
    })
    
    output$median_delta <- renderText({
      paste("Median % Missprediction:", round(median_delta, 4))
    })
    
  })
  
  
  # ==========================================================
  # 3. PREDICTION SECTION (UNDEPENDENT)
  # ==========================================================
  
  output$prediction_output <- renderText({
    sum(input$input1,
        input$input2,
        input$input3,
        input$input4,
        input$input5)
  })
  
}

shinyApp(ui, server)