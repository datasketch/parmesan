library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(tidyverse)
library(homodatum)

ui <- panelsPage(
  panel(
    title = "Upload Data",
    width = 300,
    body = div(
      uiOutput("controls")
    )
  ),
  panel(
    title = "Viz",
    body = uiOutput("result"),
    footer = uiOutput("viz_icons")
  )
)

server <-  function(input, output, session) {

  path <- system.file("examples", "test-app", "parmesan",
                      package = "parmesan")
  parmesan <- parmesan_load(path, debug = TRUE)

  parmesan_input <- parmesan_watch(input, parmesan)

  output_parmesan("all_controls_here", parmesan = parmesan,
                  input = input, output = output)

  output$debug <- renderPrint({
    str(parmesan_input())
  })

  frtype <- reactive({

  })

  parmesan_alert(parmesan, env = environment())

  data <- reactive({
    # if(input$viz_selection %in% c("pie", "donut", "bar", "treemap", "line")){
    #   data <- sample_data("Cat-Num")
    # }
    # if(input$viz_selection %in% c("grouped_bars", "stacked_bars")){
    #   data <- sample_data("Cat-Cat-Num")
    # }

    data <- sample_data("Cat-Cat-Num")

  })


  output$viz <- renderPlot({
    selected_viz <- input$viz_selection
    viz <- paste0("gg_", selected_viz, "_CatCatNum")
    data <- data()
    do.call(viz, list(data, parmesan_input()))
  })

  output$viz_icons <- renderUI({
    buttonImageInput('viz_selection',
                     HTML('<div class = "style_section">Choose a visualization type</div>'),
                     images = c("bar",  "pie", "donut", "treemap", "bubbles", "line"),
                     path = 'img/svg/',
                     format = 'svg')
  })



}


shinyApp(ui, server)
