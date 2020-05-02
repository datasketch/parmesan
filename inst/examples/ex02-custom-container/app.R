library(shiny)
library(parmesan)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  h3("This example shows a layout for input groups with custom container functions."),
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

  path <- system.file("examples", "ex02-custom-container", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  output$debug <- renderPrint({
    parmesan_input_ids(parmesan = parmesan)
  })

  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  output$controls <- renderUI({
    render_section(section = "controls", parmesan = parmesan)
  })

  output$controls2 <- renderUI({
    render_section(section = "controls_dark", parmesan = parmesan,
                   container_section = div_dark)
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
