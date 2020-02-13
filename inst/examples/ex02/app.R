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
    config_path <- system.file("examples", "ex02", "parmesan", package = "parmesan")
    parmesan_render_ui(section = "Controls", config_path = config_path)
  })

  output$controls2 <- renderUI({
    config_path <- system.file("examples", "ex02", "parmesan", package = "parmesan")
    parmesan_render_ui(section = "Controls Dark", config_path = config_path,
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
