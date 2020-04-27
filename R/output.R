
#' @export
output_parmesan <- function(selector, parmesan = NULL,
                            input = input, output = output,
                            container_section = NULL,
                            env = NULL){
  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }
  sections <- names(parmesan)

  #insert section placeholders
  lapply(sections, function(section){
    insertUI(selector, ui = div(class = "section", id = paste0("section-",section)))
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
          render_par_input(par_input, input = input, env = env)
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


