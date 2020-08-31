library(shiny)
library(parmesan)

ui <- fluidPage(
  titlePanel("Hello Parmesan!"),
  h3("This example shows dynamic inputs loaded from a YAML config file."),
  sidebarLayout(
    sidebarPanel(
      uiOutput("controls_container"),
      verbatimTextOutput("debug"),
      numericInput("min_bins", "Update Min Bins", 5)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <-  function(input, output, session) {

  path <- system.file("examples", "ex09-presets", "parmesan", package = "parmesan")

  parmesan <- reactive({
    presets <- list(
      bins = list(min = input$min_bins), # update from 5 in inputs.yaml
      column = list(label ="COL") # update label in inputs.yaml
    )
    parmesan_load(path, presets = presets)
  })


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

  parmesan_alert(parmesan, env = environment())
}


shinyApp(ui, server)
