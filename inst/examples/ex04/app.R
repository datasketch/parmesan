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


config_path <- system.file("examples", "ex04", "parmesan", package = "parmesan")
input_ids <- parmesan_input_ids(config_path = config_path)
input_ids_values <- lapply(input_ids, function(i){
  NA
})
names(input_ids_values) <- input_ids


server <-  function(input, output, session) {

  vals <- reactiveValues()
  vals$inputs <- input_ids_values
  react_env <- new.env()

  observe({
    lapply(input_ids, function(i){
      vals$inputs[[i]] <- input[[i]]
      vals
    })
  })


  output$debug <- renderPrint({
    psan_ids <- parmesan_input_ids(config_path = config_path)
    psan_ids
    datasetNCols()
    str(reactiveValuesToList(vals)$inputs)
  })

  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  datasetNCols <- reactive({
    req(datasetInput())
    ncol(datasetInput())
  }, env = react_env)

  output$controls <- renderUI({
    parmesan_render_ui(section = "Controls", config_path = config_path, input = input, env = react_env)
  })

  output$controls2 <- renderUI({
    req(datasetNCols())
    parmesan_render_ui(section = "Controls Dark", config_path = config_path,
                       container_section = div_dark, input = input, env = react_env)
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
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      plot <- hist(x, breaks = bins, col = "#75AADB", border = "white",
                   xlab = paste0("Values of ", column_name),
                   main =  paste0("This is ", dataset, ", column ", column))
    }
    plot
  })


}


shinyApp(ui, server)
