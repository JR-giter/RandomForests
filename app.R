source("globals.R")
source("Greedy_Regression_Classification.R")
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
    
    # reset global node counter (VERY IMPORTANT)
    .node_id_counter <<- 0
    
    # generate data depending on slider
    data <- createSinDataExample(input$n)
    
    input_data <- matrix(data$x, ncol = 1)
    target_variable <- data$y
    
    # build tree
    tree <- greedy_cart_regression(input_data, target_variable)
    
    # plot
    plot_cart_tree(tree)
    
  })
}

shinyApp(ui, server)
