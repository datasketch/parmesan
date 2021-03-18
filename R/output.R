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
      lapply(section$inputs, function(par_input){

        if(!(input_has_show_if(par_input) | input_has_reactive_tooltip_text(par_input))){
          insertUI(paste0("#",section$id),
                   immediate = TRUE,
                   ui = div(id = paste0("output_",par_input$id),
                            render_par_input(par_input = par_input, parent = session)))
        }

      })
    })

  })


  # Update inputs that have reactive values
  observe({
    if(shiny::is.reactive(parmesan))
      parmesan <- parmesan()

    lapply(parmesan, function(section){
      lapply(section$inputs, function(par_input){
        if(input_has_reactive_param_values(par_input)){
          # Evaluate reactives parameters that need to change with reactives
          params <-  par_input$input_params
          pars <- names(Filter(function(x) grepl("\\(\\)", x), params))
          params_reactive <- lapply(pars, function(par){
            reactive_input <- par_input$input_params[[par]]
            evaluate_reactive(x = reactive_input, env = env, r = r)
          })
          names(params_reactive) <- pars

          # Update parameters
          id <- par_input$id
          input_type <- par_input$input_type
          updateInput_type_with_ns <- updateInput_namespace(input_type)

          update_params <- c(session = session,
                             inputId = id,
                             params_reactive)
          do.call(getfun(updateInput_type_with_ns), update_params)

        }
      })
    })
  })



  # Insert/remove conditional inputs
  observe({
    if(shiny::is.reactive(parmesan))
      parmesan <- parmesan()

    lapply(parmesan, function(section){
      lapply(seq_along(section$inputs), function(x){
        par_input <- section$inputs[[x]]

        if(input_has_show_if(par_input)){

          conditions_passed <- validate_show_if(par_input = par_input, input = input, env = env, parent = session, r = r, debug = debug)

          last_input <- section$inputs[[x-1]]
          last_input_id <- paste0("output_", last_input$id)
          last_input_id_div <- paste0("#",last_input_id)

          if(conditions_passed){
            removeUI(selector = paste0("#output_",par_input$id), immediate = TRUE)
            insertUI(last_input_id_div,
                     where = "afterEnd",
                     immediate = TRUE,
                     ui = div(id = paste0("output_",par_input$id),
                              render_par_input(par_input = par_input, parent = session)))
          } else {
            removeUI(selector = paste0("#output_",par_input$id), immediate = TRUE)
          }

        }
      })
    })
  })

  # Insert inputs with reactive infotooltip
  observe({
    if(shiny::is.reactive(parmesan))
      parmesan <- parmesan()

    lapply(parmesan, function(section){
      lapply(seq_along(section$inputs), function(x){
        par_input <- section$inputs[[x]]

        if(input_has_reactive_tooltip_text(par_input)){

          text <-  par_input$input_info$text
          if(is.null(r)){
            text <- do.call(remove_parenthesis(text), list(), envir = env)
          } else {
            text <- do.call(r[[remove_parenthesis(text)]], list())
          }
          par_input$input_info$text <- text

          last_input <- section$inputs[[x-1]]
          last_input_id <- paste0("output_", last_input$id)
          last_input_id_div <- paste0("#",last_input_id)

          removeUI(selector = paste0("#output_",par_input$id), immediate = TRUE)
          insertUI(last_input_id_div,
                   where = "afterEnd",
                   immediate = TRUE,
                   ui = div(id = paste0("output_",par_input$id),
                            render_par_input(par_input = par_input, parent = session)))
        }
      })
    })
  })


  # Add parmesan_updated input
  insertUI(paste0("#",id), immediate = TRUE, ui = uiOutput("parmesan_update_output"))
  output$parmesan_update_output <- renderUI({
    shiny::tags$script("Shiny.onInputChange('parmesan_updated',+new Date);")
  })

  # })


}


