library(shiny)
library(parmesan)

ui <- fluidPage(
  titlePanel("Example 05 - Hello Parmesan!"),
  h3("Children input elements do not need to be rendered as independent outputs."),
  column(4,
         uiOutput("all_controls_here"),
         verbatimTextOutput("debug")
  ),
  column(8,
         plotOutput("distPlot")
  )
)

div_dark <- function(...){
  div(style="background-color:#f4f4f7;border: 1px solid #CCC;border-radius:10px;padding:10px;margin-bottom:10px;", ...)
}


server <-  function(input, output, session) {

  path <- system.file("examples", "ex05-reactive-output", "parmesan",
                      package = "parmesan")
  parmesan <- parmesan_load(path)

  # Put all parmesan inputs in reactive values
  parmesan_input <- parmesan_watch(input, parmesan)

  output_parmesan("all_controls_here", parmesan = parmesan,
                  container_section = div_dark,
                  input = input, output = output, env = environment())

  output$debug <- renderPrint({
    str(parmesan_input())
  })

  datasetInput <- reactive({
    req(input$dataset)
    get(input$dataset)
  })

  datasetNCols <- reactive({
    req(datasetInput())
    ncol(datasetInput())
  })

  datasetNColsLabel <- reactive({
    paste0("Colums (max = ", datasetNCols(),")")
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

  parmesan_alert(parmesan, env = environment())

}


shinyApp(ui, server)
