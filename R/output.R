
#' @export
output_parmesan <- function(id,
                            r = NULL,
                            parmesan = NULL,
                            container_section = NULL,
                            parent = NULL,
                            env = parent.frame(),
                            panic = FALSE,
                            debug = FALSE){

  moduleServer(id, function(input, output, session){
    ns <- parent$ns

  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }

  # observe({
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
      section_id <- id
      if(!is.null(parent)){
        section_id <- ns(id)
      }
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


    # Do we first need to insert individual inputs
    # with no dependencies in each section? Not for now.

    # # First insert all inputs with no dependencies
    # lapply(parmesan, function(section){
    #   lapply(section$inputs, function(par_input){
    #     if(!input_has_dependencies(par_input)){
    #       insertUI(paste0("#section-",section$id),
    #                ui = render_par_input(par_input, input = input, env = env)
    #       )
    #     }
    #   })
    # })


    # Second insert all inputs with dependencies
    # Create outputs for all inputs
    # (only those with dependencies?, not for now)

    # lapply(parmesan, function(section){
    #   lapply(section$inputs, function(par_input){
    #     # if(input_has_dependencies(par_input)){
    #     output[[paste0("output_",par_input$id)]] <- renderUI({
    #       # render_par_input(par_input = par_input, input = input, env = env, debug = debug)
    #       selectInput(par_input$id, label = "blahblah", choices = c("rock", "pressure"))
    #     })
    #     # }
    #   })
    # })

    # Create UIs for all inputs without conditionals
    lapply(parmesan, function(section){
      lapply(section$inputs, function(par_input){

        if(!input_has_show_if(par_input)){
          insertUI(paste0("#",section$id),
                   immediate = TRUE,
                   ui = div(render_par_input(par_input = par_input, input = input, env = env, debug = debug, parent = parent, r = r)))
        }

      })
    })

    observe({
      lapply(parmesan, function(section){
        lapply(section$inputs, function(par_input){
          # Update inputs that have reactive values
          if(input_has_reactive_param_values(par_input)){
            # Evaluate reactives parameters that need to change with reactives
            params <-  par_input$input_params
            pars <- names(Filter(function(x) grepl("\\(\\)", x), params))
            params_reactive <- lapply(pars, function(par){
              inp <- par_input$input_params[[par]]
              dep_value_params <- do.call(r[[remove_parenthesis(inp)]], list())

              dep_value_params
            })
            names(params_reactive) <- pars

            # Update parameters
            id <- par_input$id
            input_type <- par_input$input_type
            input_type_with_ns <- updateInput_namespace(input_type)

            update_params <- c(session = parent,
                               inputId = id,
                               params_reactive)
            do.call(getfun(input_type_with_ns), update_params)

          }
        })
      })
    })

    observe({
      lapply(parmesan, function(section){
        lapply(seq_along(section$inputs), function(x){
          par_input <- section$inputs[[x]]
          # Insert/remove conditional inputs
          if(input_has_show_if(par_input)){

            conditions_passed <- validate_show_if(par_input = par_input, input = input, env = env, parent = parent, r = r, debug = debug)

            last_input <- section$inputs[[x-1]]
            last_input_id <- last_input$id
            last_input_id_with_ns <- paste0("#",ns(last_input_id))

            if(conditions_passed){
              insertUI(last_input_id_with_ns,
                       immediate = TRUE,
                       ui = div(id = paste0("output_",par_input$id),
                                render_par_html(par_input = par_input, parent = parent)))
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

  })

  # })

}


