library(shiny)
library(parmesan)
library(shi18ny)
library(shinyjs)

ui <- fluidPage(
  titlePanel(ui_("hello")),
  h3(ui_("app_intro_i18n")),
  useShi18ny(),
  h3(ui_("app_intro")),
  langSelectorInput("lang", position = "fixed"),
  column(4,
         uiOutput("controls"),
         hr(),
         uiOutput("controls2"),
         hr(),
         verbatimTextOutput("debug")
  ),
  column(8,
         uiOutput("dynamic"),
         plotOutput("distPlot")
  )
)

div_dark <- function(...){
  div(style="background-color:#f4f4f7;border: 1px solid #CCC;border-radius:10px;padding:10px;margin-bottom:10px;", ...)
}


server <-  function(input, output, session) {

  i18n <- list(
    defaultLang = "en",
    availableLangs = c("es","en")
  )
  lang <- callModule(langSelector,"lang", i18n = i18n, showSelector=TRUE)

  path <- system.file("examples", "ex03-shi18ny", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })

  output$debug <- renderPrint({
    lang()
  })

  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  output$controls <- renderUI({

    config_path <- system.file("examples", "ex03", "parmesan", package = "parmesan")

    list(
      h3(i_("control_text.first.intro", lang())),
      render_section(section = "controls", parmesan = i_(parmesan, lang()))
    )
  })

  output$controls2 <- renderUI({

    list(
      h3(i_("control_text.second.intro", lang())),
      render_section(section = "controls_dark", parmesan = i_(parmesan, lang()),
                     container_section = div_dark)
    )
  })

  output$dynamic <- renderUI({

    l <- list(
      a = "a_text",
      b = "b_text",
      c = "c_text"
    )

    lapply(l, function(x){
      p(i_(x, lang()), style = "color:blueviolet")
    })

  })

  output$distPlot <- renderPlot({
    dataset  <- input$dataset
    column <- input$column
    x <- datasetInput()[, column]
    column_name <- names(datasetInput())[column]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = paste0("Values of ", column_name),
         main =  paste0("This is ", dataset, ", column ", column)
    )

  })


}


shinyApp(ui, server)
