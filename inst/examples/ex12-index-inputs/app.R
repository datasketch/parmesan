library(shiny)
library(shinypanels)
library(parmesan)
library(dplyr)
library(homodatum)

ui <- panelsPage(
  panel(title = "Filters",
        id = "azul",
        body = div(
          uiOutput("testInp"),
          uiOutput("indexTest"),
          uiOutput("controls"),
          verbatimTextOutput("checkDeb")
        )
  )
)

server <- function(input, output, session) {


  data_facke <- reactive({
    df <- homodatum::sample_data("Cat-Cat-Cat-Cat-Cat-Cat", 300, addNA = F)
    names(df) <- c("animales", "genero", "autos", "edad", "categoria", "tipos")
    df
  })


  dataOrigin <- reactive({
    req(data_facke())
    df <- data_facke()
    list(
      "animales" =  df %>% dplyr::group_by(animales) %>% dplyr::summarise(totalC = dplyr::n())%>% dplyr::mutate(labelCero = paste0(animales, "( 0 )")),
      "genero" =  df %>% dplyr::group_by(genero) %>% dplyr::summarise(totalC = dplyr::n())%>% dplyr::mutate(labelCero = paste0(genero, "( 0 )"))

    )
  })


  labelVal <- reactiveValues(change = NULL)
  dataVal <- reactiveValues(listD = NULL)

  observe({
    req(data_facke())
    df <- data_facke()
    if (nrow(df) == 0) return()
    df_l <- NULL
    if (!is.null(input$animalesCheck)) {
      df <- df %>% dplyr::filter(animales %in% input$animalesCheck)
    } else {
      df <- df %>% dplyr::filter(is.na(animales))
    }
    if (!is.null(input$generoCheck)) {
      df <- df %>% dplyr::filter(genero %in% input$generoCheck)
    } else {
      df <- df %>% dplyr::filter(is.na(genero))
    }

    if (is.null(df) | nrow(df) == 0) {
      dataVal$listD <- NULL
    } else {
      dataVal$listD <- list(
        "animales" =  dataOrigin()$animales %>% dplyr::left_join(df %>% dplyr::group_by(animales) %>% dplyr::summarise(total = dplyr::n()) %>% dplyr::mutate(label = paste0(animales, "(", total, ")"))) %>% dplyr::mutate(allLabel = coalesce(label, labelCero)) ,
        "genero" =  dataOrigin()$genero %>% dplyr::left_join(df %>% dplyr::group_by(genero) %>% dplyr::summarise(total = dplyr::n()) %>% dplyr::mutate(label = paste0(genero, "(", total, ")"))) %>% dplyr::mutate(allLabel = coalesce(label, labelCero))
      )
    }
    #   print(dataVal$listD)
    # if (is.null(dataVal$listD)) {
    #   labelVal$change <- list(
    #     "animales" = paste0(unique(dataOrigin()$animales$animales), " 0 "),
    #     "genero" = paste0(unique(dataOrigin()$genero$genero), " 0 ")
    #   )
    # } else {
    ls <- list(
      dataVal$listD$animales$allLabel,
      dataVal$listD$genero$allLabel
    ) %>% plyr::compact()

    if (identical(ls, list())) {
      labelVal$change <- NULL
    } else {
      names(ls) <- c("animales", "genero")
      labelVal$change <- ls
    }
   print(labelVal$change)
  })


  observe({
    if (is.null(labelVal$change)) return()

    shinyWidgets::updatePrettyCheckboxGroup(
      session = session,
      "animalesCheck",
      choiceNames = labelVal$change$animales,
      choiceValues = paste0(gsub("\\s*\\([^\\)]+\\)","",labelVal$change$animales)),
      selected =  input$animalesCheck
    )

    shinyWidgets::updatePrettyCheckboxGroup(
      session = session,
      "generoCheck",
      choiceNames = labelVal$change$genero,
      choiceValues = paste0(gsub("\\s*\\([^\\)]+\\)","",labelVal$change$genero)),
      selected =  input$generoCheck
    )
  })

  aniOpts <- reactive({
   # req(data_facke()
  #  paste0(unique(data_facke()$animales), " test")
   # if (is.null(labelVal$change$animales)) {
      paste0(unique(data_facke()$animales), " test")
    # } else {
    # labelVal$change$animales
    # }
  })

  aniVal <- reactive({
    unique(data_facke()$animales)
  })

  genOpts <- reactive({
    paste0(unique(data_facke()$genero), " test")
  })

  genVal <- reactive({
    unique(data_facke()$genero)
  })


  # output$testInp <- renderUI({
  #   checkboxGroupInput("lala", "hola",  choiceNames =
  #                        list(icon("calendar"), icon("bed"),
  #                             icon("cog"), icon("bug")),
  #                      choiceValues =
  #                        list("calendar", "bed", "cog", "bug"))
  # })
  #
  #
  #  dfTest <- reactive({
  #    c("a", "b", "c", "d", "e", "f")
  #  })
  #
  #  dfNames <- reactive({
  #    c("a1", "a1", "a1", "d4", "e5", "f7")
  #  })
  #
  #  output$checkDeb <-  renderPrint({
  #    input$testCheck
  #  })
  #
  #  output$indexTest <- renderUI({
  #
  #
  #    parmesan::indexButtonsUI(id = "INDEXTEST", label = "filtros aplicados", img_icon = "close.svg",
  #                             list_inputs = li(), dic_yaml = yaml::read_yaml("parmesan/choices_dic.yaml"))
  #
  #  })
  #
  #  data_opts <- reactive({
  #    c("rock", "pressure", "cars", "iris")
  #  })
  #
  #
  #
  #  agg_palette <- reactive({
  #    palette_a <- div(
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #17BEBB;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #2E282A;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #CD5334;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #EDB88B;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #FAD8D6;"),
  #    )
  #
  #    palette_b <- div(
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #515A47;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #D7BE82;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #7a4419;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #755C1B;"),
  #      div(style="width: 20px; height: 20px; display: inline-block; background-color: #400406;"),
  #    )
  #    list(palette_a = as.character(palette_a), palette_b = as.character(palette_b))
  #
  #  })
  #
  #  agg_sel <- reactive({
  #    req(agg_palette())
  #   "palette_a"
  #  })
  #
  #
  #  #indexButtonsServer(session = session, input = input, id = "INDEXTEST")
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("controls",
                  input = input,
                  output = output,
                  session = session,
                  env = environment())

  #
  #  li <- reactive({
  #    parmesan:::index_inputs(session = session, input = input) %>% plyr::compact()
  #  })
  #  indexButtonsServer(session = session, input = input, id = "INDEXTEST")

}


shinyApp(ui, server)

