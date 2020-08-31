library(shiny)
library(parmesan)

ui <- fluidPage(icon("check"),
                titlePanel("Hello Parmesan!"),
                h3("This example shows information tooltips it's styles for inputs"),
                textInput("new_tooltip", "New Bins Tooltip", value = "INIT TOOLTIP"),
                uiOutput("controls_container"))

server <-  function(input, output, session) {

  path <- system.file("examples", "ex08-info-tooltips", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  output$controls_container <- renderUI({
    render_section(parmesan = parmesan)
  })

  custom_bins_text <- reactive({
    input$new_tooltip
  })

}


shinyApp(ui, server)
