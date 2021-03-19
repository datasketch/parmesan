#' @export
output_parmesan <- function(id,
                            parmesan,
                            input,
                            output,
                            session,
                            container_section = NULL,
                            r = NULL,
                            env = parent.frame(),
                            panic = FALSE, debug = FALSE){


  if(is.null(parmesan)) stop("Need 'parmesan' parameter to create shiny inputs.")
  if(is.null(input)) stop("Need 'input' parameter to create shiny inputs.")
  if(is.null(output)) stop("Need 'output' parameter to create shiny inputs.")
  if(is.null(session)) stop("Need 'session' parameter to create shiny inputs.")

  ns <- session$ns

  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }

  # Initialise sections and inputs (insert all without reactives or conditions)
  observe({
    if(shiny::is.reactive(parmesan))
      parmesan <- parmesan()

    # For some reason this is needed so the env gets "loaded"
    # and it gets passed to all functions that need it
    fenv(env, "OUTPUT", silent = TRUE)

    sections <- names(parmesan)

    #insert section placeholders
    lapply(sections, function(section){
      removeUI(selector = paste0("#section-",section), immediate = TRUE)
      section_id <- ns(id)
      insertUI(paste0("#",section_id), immediate = TRUE,
               ui = div(class = "section_0", id = paste0("section-",section)))
    })

    # Insert sections titles and boxes leaving out inputs.
    lapply(sections, function(section){
      insertUI(paste0("#section-",section), immediate = TRUE,
               ui = render_section(section = section, parmesan = parmesan,
                                   container_section = container_section,
                                   input = input,
                                   render_inputs = FALSE,
                                   env = env))
    })

    # Create UIs for all inputs without conditionals
    lapply(parmesan, function(section){
      lapply(seq_along(section$inputs), function(x){
        par_input <- section$inputs[[x]]

        observe({

          if(section$inputs[[1]]$id == par_input$id){
            selector <- paste0("#",section$id)
            where <- "beforeEnd"
          } else {
            last_input <- section$inputs[[x-1]]
            last_input_id <- paste0("output_", last_input$id)
            selector <- paste0("#",last_input_id)
            where <- "afterEnd"
          }

          removeUI(selector = paste0("#output_",par_input$id), immediate = TRUE)
          insertUI(selector = selector,
                   where = where,
                   immediate = TRUE,
                   ui = div(id = paste0("output_",par_input$id),
                            render_par_input(par_input = par_input, input = input, env = env, parent = session, r = r, debug = debug)))
        })
      })
    })
  })


  # Add parmesan_updated input
  insertUI(paste0("#",id), immediate = TRUE, ui = uiOutput("parmesan_update_output"))
  output$parmesan_update_output <- renderUI({
    shiny::tags$script("Shiny.onInputChange('parmesan_updated',+new Date);")
  })


}


