library(shiny)
library(shinypanels)
library(parmesan)



ui <- panelsPage(
  panel(title = "Filters",
        id = "azul",
        body = div(
          verbatimTextOutput("test"),
          parmesan::resetButtonUI(id = "reset_button", icon = "close"),
          uiOutput("controls")
        )
  )
)

server <- function(input, output, session) {




  parmesan::resetButtonServer(id = "reset_button",
                              input = input,
                              id_reset = "all",#c("plot_type", "bins"),
                              session=session)

  agg_palette <- reactive({
    c("#AAF13C", "#DD77CC", "#FF33AA")
  })

  data_opts <- reactive({
    c("rock", "pressure", "cars", "iris")
  })


  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input,
                  output = output,
                  session = session,
                  env = environment())

  output$test <- renderPrint({
    list(
    input$dataset,
    input$bins
    )
  })


}


shinyApp(ui, server)

