library(shinypanels)
library(parmesan)
library(shinyinvoer)


ui <- panelsPage(
  panel(
    title = "Upload Data",
    width = 300,
    body =  h3("Upload data"),
    collapsed = TRUE,
  ),
  panel(
    title = "Dataset",
    width = 400,
    collapsed = TRUE,
    body =  h3("Dataset")
  ),
  panel(
    title = "Edit viz",
    width = 350,
    body = div(
      selectInput("selected_ftype", "Ftype", c("Cat", "Cat-Num")),
      uiOutput("controls")
    )
  ),
  panel(
    title = "Viz",
    body = div(
      plotOutput("vizView"),
    ),
    footer = uiOutput("viz_icons")
  )
)

config_path <- "parmesan"

# Reactive part
input_ids <- parmesan_input_ids(config_path = config_path)
input_ids_values <- lapply(input_ids, function(i){
  NA
})
names(input_ids_values) <- input_ids


server <-  function(input, output, session) {

  react_env <- new.env()

  output$controls <- renderUI({
    list(
      parmesan_render_ui(section = c("Format plot"), config_path = config_path, input = input, env = react_env)
      ,
      parmesan_render_ui(section = "Chart titles", config_path = config_path, input = input, env = react_env)

    )
  })

  output$viz_icons <- renderUI({
    list(
      parmesan_render_ui(section = c("Viz types"), config_path = config_path, input = input, env = react_env)
    )
  })

  ftype <- reactive({
    input$selected_ftype
  })

  ftype_image_recommendation <- reactive({
    if(ftype() == "Cat"){
      c("pie", "bar")
    } else{
      c("bar", "treemap")
    }
  }, env = react_env)

  output$vizView <- renderPlot({
    plot(cars[[1]])
  })

}


shinyApp(ui, server)
