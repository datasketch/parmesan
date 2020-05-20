library(shiny)
library(parmesan)

styles <- "

.tlt {
  background: #eee;
  /*box-shadow: 3px 3px 8px 3px rgba(0, 0, 0, 0.06);
  display: inline-block;*/
  left: 20px;
  max-width: 200px;
  min-width: 200px;
  padding: 7px 10px;
  position: absolute;
  top: -12px;
  visibility: hidden;
  /*text-align: center;*/
  z-index: 100;
}

.param_info {
  cursor: pointer;
  display: inline-block;
}

.param_info:hover + div {
  visibility: visible
}

"

ui <- fluidPage(tags$style(styles),
  titlePanel("Hello Parmesan!"),
  h3("This example shows dynamic inputs loaded from a YAML config file."),
  sidebarLayout(
    sidebarPanel(
      uiOutput("controls_container"),
      verbatimTextOutput("debug")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <-  function(input, output, session) {

  # path <- system.file("examples", "ex01-basic", "parmesan", package = "parmesan")
  # parmesan <- parmesan_load(path)
  parmesan <- parmesan_load("parmesan")
  # parmesan <- parmesan_load("inst/examples/ex01-basic/parmesan")

  output$debug <- renderPrint({
    parmesan_input_ids(parmesan = parmesan)
  })

  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  output$controls_container <- renderUI({
    render_section(parmesan = parmesan)
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
