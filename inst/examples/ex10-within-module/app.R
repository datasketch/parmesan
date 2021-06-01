library(shiny)
library(parmesan)
library(shinypanels)

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
      ns <- NS(id)


      # Initialise parmesan inputs
      path <- system.file("examples", "ex10-within-module", "parmesan",
                          package = "parmesan")

      parmesan <- parmesan_load(path)

      parmesan_input <- parmesan_watch(input, parmesan)

      output_parmesan("all_controls_here", r = r, parmesan = parmesan,
                      input = input, output = output, session = session)

      # Define reactives needed in parmesan
      dataset_choices <- reactive({
        c("rock", "pressure", "cars")
      })


      plot_type_choices <- reactive({
        c("Plot", "Histogram")
      })

      plot_type_selected <- reactive({
        req(plot_type_choices())
        plot_type_choices()[2]
      })

      bins_default_value <- reactive({
        5
      })

      datasetInput <- reactive({
        req(input$dataset)
        if(any(grepl("\\(\\)", input$dataset))) return()
        get(input$dataset)
      })

      datasetNCols <- reactive({
        req(datasetInput())
        ncol(datasetInput())
      })

      datasetNColsLabel <- reactive({
        paste0("Colums (max = ", datasetNCols(),")")
      })

      colourCustomChoices <- reactive({
        paletero::paletero_cat(LETTERS[1:8], palette = "Set1")
      })

      maxCustomChoices <- reactive({
        if(is.null(input$plot_type)){
          2
        } else if(input$plot_type == "Histogram"){
          1
        } else {
          4
        }
      })

      observe({
        r$plot_type_choices <- plot_type_choices()
      })

      observe({
        r$plot_type_selected <- plot_type_selected()
      })

      observe({
        r$bins_default_value <- bins_default_value()
      })

      observe({
        r$dataset_choices <- dataset_choices()
      })

      observe({
        r$maxCustomChoices <- maxCustomChoices()
      })

      observe({
        r$colourCustomChoices <- colourCustomChoices()
      })

      observe({
        r$datasetNColsLabel <- datasetNColsLabel()
      })

      observe({
        r$datasetNCols <- datasetNCols()
      })

      observe({
        r$datasetInput <- datasetInput()
      })

      observe({
        parmesan_alert(parmesan, r = r, env = environment())
      })


      # Pass all inputs from parmesan to other parts of the app as reactiveValues
      parmesan_inputs <- purrr::map(parmesan, function(.x) { purrr::map_chr(.x$inputs, "id")}) %>% unlist(use.names = FALSE)

      observe({
        for(parmesan_input in parmesan_inputs){
          get_input <- input[[parmesan_input]]
          if(!is.null(get_input)){
            r[[parmesan_input]] <- get_input
          }
        }
      })

      # Hide one of the inputs with modal
      observe({
        shinypanels::showModalMultipleId(modal_id = "modal_plan_controls", list_id = c(ns("output_plot_type")))
      })

      output$debug <- renderPrint({
        str(parmesan_input())
      })

    }
  )
}

ui <- panelsPage(
  shinypanels::modal(id = 'modal_plan_controls', title = "modal_upgrade_title", "message_modal_controls"),
  # titlePanel("Example 10 - Hello Parmesan!"),
  # h3("Use parmesan inputs from within a shiny module."),
  panel(title = "Example 10 - Hello Parmesan!",
        collapse = FALSE,
        width = 300,
        body = parmUI("parm_module")),
  panel(title = "Plot",
        body = plotOutput("distPlot"))
)

div_dark <- function(...){
  div(style="background-color:#f4f4f7;border: 1px solid #CCC;border-radius:10px;padding:10px;margin-bottom:10px;", ...)
}

server <-  function(input, output, session) {

  r <- reactiveValues()

  parmServer("parm_module", r = r)

  output$distPlot <- renderPlot({
    req(r$dataset, r$column, r$datasetInput, r$colour_custom)
    if(r$dataset == " ") return()

    dataset  <- r$dataset
    column <- r$column
    x <- r$datasetInput[, column]
    column_name <- names(r$datasetInput)[column]


    if(r$plot_type == "Plot"){
      plot <- plot(x, col = r$colour_custom)
    }
    if(r$plot_type == "Histogram"){
      req(r$bins)
      bins <- seq(min(x), max(x), length.out = r$bins + 1)
      plot <- hist(x, breaks = bins, col = r$colour_custom, border = "white",
                   xlab = paste0("Values of ", column_name),
                   main =  paste0("This is ", dataset, ", column ", column))
      # plot <- hist(x)
    }
    plot
  })

}


shinyApp(ui, server)
