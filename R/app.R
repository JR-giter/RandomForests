source("globals.R")
source("Greedy_Cart.R")
source("Plotting_Trees.R")
source("Test_Hub.R")
source("AlgorithmApplication.R")

# Code for UI
ui <- page_sidebar(
  title = "Greedy CART Regression",
  
  sidebar = sidebar(
    sliderInput(
      inputId = "n",
      label = "Number of data points:",
      min = 5,
      max = 200,
      value = 20,
      step = 1
    )
  ),
  
  plotOutput("treePlot", height = "600px")
)

server <- function(input, output, session) {
  
  output$treePlot <- renderPlot({
    

    # reset global node counter
    .node_id_counter <<- 0

    .node_id_counter <<- 0  # Counter für IDs zurücksetzen
    
    
    tree <- GreedyCart(dataSet = ames,
                       n_properties = 3,
                       n_nodes = input$n,
                       mode = "regression",
                       target = "Sale_Price")
    # plott tree
    plot_cart_tree(tree)
  })
}
shinyApp(ui, server)