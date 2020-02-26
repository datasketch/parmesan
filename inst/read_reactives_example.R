

ui <- fluidPage(
  radioButtons("paper", "Select", c("paper 1", "paper 2")),
  radioButtons("rock", "Select", c("rock 1", "rock 2")),
  selectInput("selection", "Select", c("scissors","paper", "rock")),
  verbatimTextOutput("debug"),
  uiOutput("results")
)
server <- function(input, output, session){

  react_env <- new.env()

  scissors <- reactive({
    input$paper
    input$rock
    paste0("scissors", Sys.time())
  }, env = react_env)

  output$debug <- renderPrint({
    input[[input$selection]]
    render_custom(input$selection, input)

    # str(input)
    # str(input[["scissors"]])
    string <- "scissors"
    # x <- do.call(string, list())
    # x
    # ls("scissors", envir = parent.env(parent.env(parent.frame())))
    ls(envir = react_env)
    #environmentName(pryr::where(string))
  })


  output$results <- renderUI({
    # render_custom(input$selection, input = input)


    string <- input$selection
    # do.call(string, list(), envir = parent.frame())
    # do.call(string, list(), envir = parent.env(parent.frame()))
    str(string)
    do.call(string, list(), envir = react_env)

  })
}

render_custom <- function(string, input = input){

  #if(shiny::is.reactive())

  if(!is.null(input[[string]])){
    x <- input[[string]]
  } else {
    string <- "scissors"
    string <- input$selection
    x <- do.call(string, list(), envir = parent.env(parent.frame()))
    # x <- do.call(string, list(), envir = parent.env(parent.env(pryr::where(string))))
  }
  h3(x)

}

shinyApp(ui, server)
