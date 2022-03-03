library(shiny)
library(shinypanels)
library(parmesan)



ui <- panelsPage(
  panel(title = "Filters",
        id = "azul",
        body = div(
          uiOutput("indexTest"),
          #verbatimTextOutput("test"),
          uiOutput("controls")
        )
  )
)

server <- function(input, output, session) {




  li <- reactive({
    parmesan:::index_inputs(session = session, input = input) %>% plyr::compact()
  })

  output$indexTest <- renderUI({

    indexButtonsUI(id = "INDEXTEST", list_inputs = li(),
                   dic = data.frame(id = c("plot_type", "bins", "dataset"),
                                    label = c("Grafico", "Bins", "Datos")))
  })

  data_opts <- reactive({
    c("rock", "pressure", "cars", "iris")
  })

  #indexButtonsServer(session = session, input = input, id = "INDEXTEST")
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input,
                  output = output,
                  session = session,
                  env = environment())

  indexButtonsServer(session = session, input = input, id = "INDEXTEST")

}


shinyApp(ui, server)

