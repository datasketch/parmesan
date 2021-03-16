library(shiny)
library(shi18ny)
library(parmesan)

ui <- fluidPage(
  useShi18ny(),
  titlePanel("Example 07 - Hello Parmesan!"),
  h3("Translations and Children input elements do not need to be rendered as independent outputs."),
  langSelectorInput("lang", position = "fixed"),
  column(4,
         uiOutput("all_controls_here"),
         verbatimTextOutput("debug")
  ),
  column(8,
         plotOutput("distPlot")
  )
)

server <-  function(input, output, session) {

  i18n <- list(
    defaultLang = "en",
    availableLangs = c("es","en")
  )

  # localeDir <- system.file("examples", "ex07-reactive-output-shi18ny", "locale", package = "parmesan")
  # opts <- list(
  #   localeDir = localeDir,
  #   defaultLang = "es",
  #   fallbacks = list("es" = "en")
  # )
  # config <- i18nConfig(opts)
  # i18n <- i18nLoad(opts)

  lang <- callModule(langSelector,"lang", i18n = i18n, showSelector=TRUE)

  path <- system.file("examples", "ex07-reactive-output-shi18ny", "parmesan",
                      package = "parmesan")
  parmesan <- parmesan_load(path)


  # parmesan_es <- i_(parmesan, "es", i18n = i18n)

  # Put all parmesan inputs in reactive values
  parmesan_input <- parmesan_watch(input, parmesan)

  parmesan_lang <- reactive({
    i_(parmesan, lang(), keys = c("label", "choices"))
  })

  output_parmesan("all_controls_here", parmesan = parmesan_lang,
                  # input = input, output = output, session = session,
                  env = environment())
  # output_parmesan("all_controls_here", parmesan = parmesan,


  output$debug <- renderPrint({
    # paste0(
    #   "Parmesan updated: ", input$parmesan_updated
    #   # str(parmesan_input())
    #   )
    str(parmesan_input())
  })

  datasetInput <- reactive({
    req(input$dataset)
    # browser()
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

    if(input$plot_type %in% c("Plot", "Gráfico")){
      plot <- plot(x)
    }
    if(input$plot_type %in% c("Histogram", "Histograma")){
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
