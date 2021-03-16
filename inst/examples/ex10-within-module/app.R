library(shiny)
library(parmesan)

parmUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("all_controls_here")),
    verbatimTextOutput(ns("debug"))
  )
}

parmServer <- function(id, r) {
  moduleServer(
    id,
    function(input, output, session) {

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

      observe({
        r$datasetNColsLabel <- datasetNColsLabel
        r$datasetNCols <- datasetNCols
      })


      path <- system.file("examples", "ex05-reactive-output", "parmesan",
                          package = "parmesan")

        parmesan <- parmesan_load(path)

        # Put all parmesan inputs in reactive values

        parmesan_input <- parmesan_watch(input, parmesan)

        parmesan_alert(parmesan, env = environment())

        output_parmesan("all_controls_here", r = r, parmesan = parmesan,
                        container_section = div_dark, parent = session)


      output$debug <- renderPrint({
        # str(parmesan_input())
        "hello"
      })

      observe({
        r$datasetInput <- datasetInput()
        r$dataset <- input$dataset
        r$column <- input$column
        r$plot_type <- input$plot_type
        r$bins <- input$bins
      })


    }
  )
}

ui <- fluidPage(
  titlePanel("Example 05 - Hello Parmesan!"),
  h3("Children input elements do not need to be rendered as independent outputs."),
  column(4,
         parmUI("parm_module")
  ),
  column(8,
         plotOutput("distPlot")
  )
)

div_dark <- function(...){
  div(style="background-color:#f4f4f7;border: 1px solid #CCC;border-radius:10px;padding:10px;margin-bottom:10px;", ...)
}

server <-  function(input, output, session) {

  r <- reactiveValues()

  parmServer("parm_module", r = r)

  output$distPlot <- renderPlot({
    req(r$dataset, r$column, r$datasetInput)
    dataset  <- r$dataset
    column <- r$column
    x <- r$datasetInput[, column]
    column_name <- names(r$datasetInput)[column]

    # browser()

    if(r$plot_type == "Plot"){
      plot <- plot(x)
    }
    if(r$plot_type == "Histogram"){
      req(r$bins)
      bins <- seq(min(x), max(x), length.out = r$bins + 1)
      plot <- hist(x, breaks = bins, col = "#75AADB", border = "white",
                   xlab = paste0("Values of ", column_name),
                   main =  paste0("This is ", dataset, ", column ", column))
    }
    plot
  })

}


shinyApp(ui, server)
