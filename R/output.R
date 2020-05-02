

output_parmesan <- function(id, parmesan = NULL,
                            input = input, output = output,
                            container_section = NULL,
                            env = parent.frame(),
                            panic = FALSE, debug = FALSE){
  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }

  # For some reason this is needed so the env gets "loaded"
  # and it gets passed to all functions that need it
  fenv(env, "OUTPUT", silent = TRUE)
  # parmesan_alert(parmesan, env = env, panic = TRUE)

  sections <- names(parmesan)

  #insert section placeholders
  lapply(sections, function(section){
    insertUI(paste0("#",id), ui = div(class = "section", id = paste0("section-",section)))
  })

  # Insert sections titles and boxes leaving out inputs.
  lapply(sections, function(section){
    insertUI(paste0("#section-",section),
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
  lapply(parmesan, function(section){
    lapply(section$inputs, function(par_input){
      # if(input_has_dependencies(par_input)){
        output[[paste0("output_",par_input$id)]] <- renderUI({
          render_par_input(par_input, input = input, env = env, debug = debug)
        })
      # }
    })
  })
  # Create UIs for all inputs with dependencies
  lapply(parmesan, function(section){
    lapply(section$inputs, function(par_input){
      # if(input_has_dependencies(par_input)){
        insertUI(paste0("#section-",section$id),
                 ui = uiOutput(paste0("output_",par_input$id)))
      # }
    })
  })
}


