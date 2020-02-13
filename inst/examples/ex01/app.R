library(shiny)
library(parmesan)

ui <- fluidPage(
  titlePanel("Hello Parmesan!"),
  h3("This example shows dynamic inputs loaded from a YAML config file."),
  sidebarLayout(
    sidebarPanel(
      uiOutput("controls"),
      verbatimTextOutput("debug")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <-  function(input, output, session) {

  output$debug <- renderPrint({
    config_path <- system.file("examples", "ex01", "parmesan", package = "parmesan")
    psan_ids <- parmesan_input_ids(config_path = config_path)
    psan_ids
  })

  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  output$controls <- renderUI({
    # sliderInput(inputId = "bins",
    #             label = "Number of bins:",
    #             min = 1,
    #             max = 50,
    #             value = 30)
    # selectInput(inputId = "dataset",
    #             label = "Choose a dataset:",
    #             choices = c("rock", "pressure", "cars")),
    # numericInput(inputId = "obs",
    #              label = "Number of observations to view:",
    #              value = 10)
    config_path <- system.file("examples", "ex01", "parmesan", package = "parmesan")
    parmesan_render_ui(config_path = config_path)
  })

  output$distPlot <- renderPlot({
    dataset  <- input$dataset
    column <- input$column
    x <- datasetInput()[, column]
    column_name <- names(datasetInput())[column]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste0("Values of ", column_name),
         main =  paste0("This is ", dataset, ", column ", column)
    )

  })


}


shinyApp(ui, server)
