
#' @export
output_parmesan <- function(id,
                            parmesan = NULL,
                            input = input,
                            output = output,
                            session = session,
                            container_section = NULL,
                            r = NULL,
                            env = parent.frame(),
                            panic = FALSE, debug = FALSE){

  ns <- session$ns

  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }

  observe({
    if(shiny::is.reactive(parmesan))
      parmesan <- parmesan()

    # For some reason this is needed so the env gets "loaded"
    # and it gets passed to all functions that need it
    fenv(env, "OUTPUT", silent = TRUE)
    # parmesan_alert(parmesan, env = env, panic = TRUE)

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

    # Second insert all inputs

    # Create UIs for all inputs without conditionals
    lapply(parmesan, function(section){
      lapply(section$inputs, function(par_input){

        if(!input_has_show_if(par_input)){
          insertUI(paste0("#",section$id),
                   immediate = TRUE,
                   ui = div(id = paste0("output_",par_input$id),
                            render_par_input(par_input = par_input, input = input, env = env, debug = debug, parent = session, r = r)))
        }

      })
    })

  })

    observe({
      if(shiny::is.reactive(parmesan))
        parmesan <- parmesan()

      lapply(parmesan, function(section){
        lapply(section$inputs, function(par_input){
          # Update inputs that have reactive values
          if(input_has_reactive_param_values(par_input)){
            # Evaluate reactives parameters that need to change with reactives
            params <-  par_input$input_params
            pars <- names(Filter(function(x) grepl("\\(\\)", x), params))
            params_reactive <- lapply(pars, function(par){
              inp <- par_input$input_params[[par]]

              if(is.null(r)){
                dep_value_params <- do.call(remove_parenthesis(inp), list(), envir = env)
              } else {
                dep_value_params <- do.call(r[[remove_parenthesis(inp)]], list())
              }


              dep_value_params
            })
            names(params_reactive) <- pars

            # Update parameters
            id <- par_input$id
            input_type <- par_input$input_type
            input_type_with_ns <- updateInput_namespace(input_type)

            update_params <- c(session = session,
                               inputId = id,
                               params_reactive)
            do.call(getfun(input_type_with_ns), update_params)

          }
        })
      })
    })

    observe({
      if(shiny::is.reactive(parmesan))
        parmesan <- parmesan()

      lapply(parmesan, function(section){
        lapply(seq_along(section$inputs), function(x){
          par_input <- section$inputs[[x]]
          # Insert/remove conditional inputs
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
                                render_par_html(par_input = par_input, parent = session)))
            } else {
              removeUI(selector = paste0("#output_",par_input$id), immediate = TRUE)
            }

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


