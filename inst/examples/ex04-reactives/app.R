library(shiny)
library(parmesan)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  h3("This example shows a layout for input groups with custom container functions
     and children input elements."),
  column(4,
         uiOutput("controls"),
         hr(),
         uiOutput("controls2"),
         hr(),
         verbatimTextOutput("debug")
  ),
  column(8,
         plotOutput("distPlot")
  )
)

div_dark <- function(...){
  div(style="background-color:#DDD;border: 2px solid #CCC;border-radius:10px;padding:10px;", ...)
}




server <-  function(input, output, session) {

  path <- system.file("examples", "ex04-reactives", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)
  parmesan_env <- new.env()

  # Put all parmesan inputs in reactive values
  parmesan_input <- parmesan_watch(input, parmesan)

  output$debug <- renderPrint({
    # str(reactiveValuesToList(parmesan_inputs))
    # datasetNCols()
    # str(reactiveValuesToList(parmesan_inputs))
    str(parmesan_input())
  })


  datasetInput <- reactive({
    req(input$dataset)
    switch(
      # parmesan_input()$dataset,
      input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  datasetNCols <- reactive({
    req(datasetInput())
    ncol(datasetInput())
  })

  datasetNColsLabel <- reactive({
    paste0("Colums (max = ", datasetNCols(),")")
  })

  output$controls <- renderUI({
    render_section(section = "controls", parmesan = parmesan)
  })

  output$controls2 <- renderUI({
    # req(datasetNCols())
    render_section(section = "controls_dark", parmesan = parmesan,
                   container_section = div_dark,
                   input = input)
  })

  output$distPlot <- renderPlot({
    req(input$dataset, input$column, datasetInput())
    dataset  <- input$dataset
    column <- input$column
    x <- datasetInput()[, column]
    column_name <- names(datasetInput())[column]

    if(input$plot_type == "Plot"){
      plot <- plot(x)
    }
    if(input$plot_type == "Histogram"){
      req(input$bins)
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      plot <- hist(x, breaks = bins, col = "#75AADB", border = "white",
                   xlab = paste0("Values of ", column_name),
                   main =  paste0("This is ", dataset, ", column ", column))
    }
    plot
  })


}


shinyApp(ui, server)
