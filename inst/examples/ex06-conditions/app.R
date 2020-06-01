library(shiny)
library(parmesan)

ui <- fluidPage(
  titlePanel("Hello Parmesan!"),
  h3("Example 6. More conditional inputs"),
  column(4,
         uiOutput("all_controls_here")
  ),
  column(8,
         plotOutput("plot"),
         verbatimTextOutput("debug")
  )
)

server <-  function(input, output, session) {

  path <- system.file("examples", "ex06-conditions", "parmesan",
                      package = "parmesan")
  parmesan <- parmesan_load(path)
  parmesan_env <- new.env()

  datasetInput <- reactive({
    req(input$dataset)
    get(input$dataset)
   })

  datasetNCols <- reactive({
    req(datasetInput())
    ncol(datasetInput())
  })

  datasetNumCols <- reactive({
    req(datasetInput())
    classes <- unlist(lapply(datasetInput(),class))
    names(datasetInput())[classes == "numeric"]
  })

  datasetFctCols <- reactive({
    req(datasetInput())
    classes <- unlist(lapply(datasetInput(),class))
    names(datasetInput())[classes == "factor"]
  })

  hasFctAndNum <- reactive({
    x <- unlist(lapply(datasetInput(), class))
    all(c("factor","numeric") %in% x)
  })

  selectedVars <- reactive({
    c(input$num_column, input$fct_column)
  })

  selectedVarsLabel <- reactive({
    paste0("Which variable to plot:",
           paste0(c(input$num_column, input$fct_column),collapse = ", ")
    )
  })

  nSelectedVars <- reactive({
    length(selectedVars())
  })

  title_selector_reactive <- reactive({
    input$title_selector
  })

  output_parmesan("all_controls_here", parmesan = parmesan,
                  input = input, output = output, env = environment(),
                  debug = TRUE)

  output$debug <- renderPrint({
    #selectedVars()
    shiny::is.reactive(input$dataset)
  })

  output$plot <- renderPlot({
    if(is.null(selectedVars()))
      return()

    if(is.null(input$plot_which)){
      plot_which <- selectedVars()[1]
    }else{
      plot_which <- selectedVars()[input$plot_which]
    }
    plot(datasetInput()[[plot_which]],
         ylab = names(datasetInput())[plot_which],
         main = paste(input$dataset, plot_which))
  })

  parmesan_alert(parmesan, env = environment(), panic = TRUE)


}

shinyApp(ui, server)
