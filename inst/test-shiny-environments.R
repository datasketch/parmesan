
environment()

ui <- fluidPage(
  p("hola"),
  selectInput("select_react","Select",1:2),
  selectInput("select1","Select",letters[1:3]),
  selectInput("select2","Select",LETTERS[1:3]),
  verbatimTextOutput("debug")
)
server <- function(input, output, session){
  str(environment())
  this.env <- environment()

  react1 <- reactive({
    input$select1
  })

  react2 <- reactive({
    input$select2
  })


  fun <- function(input){
    do.call(paste0("react",input[["select_react"]]), list())
  }

  output$debug <- renderPrint({
    fun(input)
  })

}

# get('var', envir=test.env)

shinyApp(ui, server)

