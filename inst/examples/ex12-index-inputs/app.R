library(shiny)
library(shinypanels)
library(parmesan)



ui <- panelsPage(
  panel(title = "Filters",
        id = "azul",
        body = div(
          uiOutput("indexTest"),
          uiOutput("controls")
        )
  )
)

server <- function(input, output, session) {






  output$indexTest <- renderUI({


    parmesan::indexButtonsUI(id = "INDEXTEST", label = "filtros aplicados", img_icon = "close.svg",
                             list_inputs = li(), dic_yaml = yaml::read_yaml("parmesan/choices_dic.yaml"))

  })

  data_opts <- reactive({
    c("rock", "pressure", "cars", "iris")
  })


  agg_palette <- reactive({
    palette_a <- div(
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #17BEBB;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #2E282A;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #CD5334;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #EDB88B;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #FAD8D6;"),
    )

    palette_b <- div(
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #515A47;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #D7BE82;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #7a4419;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #755C1B;"),
      div(style="width: 20px; height: 20px; display: inline-block; background-color: #400406;"),
    )
    list(palette_a = as.character(palette_a), palette_b = as.character(palette_b))

  })

  agg_sel <- reactive({
    req(agg_palette())
   "palette_a"
  })


  #indexButtonsServer(session = session, input = input, id = "INDEXTEST")
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input,
                  output = output,
                  session = session,
                  env = environment())


  li <- reactive({
    parmesan:::index_inputs(session = session, input = input) %>% plyr::compact()
  })
  indexButtonsServer(session = session, input = input, id = "INDEXTEST")

}


shinyApp(ui, server)

