

render_section <- function(section = NULL,
                           parmesan = NULL,
                           container_section = NULL,
                           container_title = NULL,
                           container_element = NULL){

  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }
  if(is.null(section)){
    section <- names(parmesan)[1]
  }

  if(is.null(container_section)){
    container_section <- shiny::tags$div
  }
  if(is.null(container_title)){
    container_title <- shiny::tags$div
  }
  if(is.null(container_element)){
    container_element <- shiny::tags$div
  }

  section <- parmesan[[section]]
  container_section(
    container_title(id = section$id, class = 'par_section', section$label),
    lapply(section$inputs, function(par_input) {
      render_input(par_input, input = input, env = env)
    })
  )

}


render_input <- function(par_input, input, env){
  message("\nRendering input: ", par_input$id, "\n")
  if(!par_input$input_type %in% available_inputs())
    stop(par_input$input_type, " is not a registered input")
  if(!par_input$show) return()

  if (is.null(par_input$depends_on)) {
    # Has no dependencies
    message("No dependencies: ", par_input$name)
    par_input$input_params$inputId <- par_input$id
    # par_input$input_params$label <- render_label(par_input$input_params$label)
    html <- do.call(par_input$input_type, par_input$input_params)
  }
  # else {
  #   # Has dependencies
  #   dependency <- list(
  #     name = names(input_list$depends_on) %||% input_list$depends_on[[1]][[1]],
  #     trigger = names(input_list$depends_on[[1]]),
  #     value = input_list$depends_on[[1]][[1]]
  #   )
  #   str(dependency)
  #   message("input[[",dependency$name,"]] is: ", input[[dependency$name]])
  #   str(input[[dependency$name]])
  #   if(!is.null(input[[dependency$name]])){
  #     # Dependency on input
  #     message("Dependency on input: ", dependency$name)
  #     dep_value <- input[[dependency$name]]
  #   } else {
  #     message("Dependency on reactive: ", dependency$name)
  #     reactive_fun <- gsub("reactive__","", dependency$name)
  #     dep_value <- do.call(reactive_fun, list(), envir = env)
  #   }
  #   message("Dep Value: ", dep_value)
  #   str(dep_value)
  #   # Single dependency case
  #   if(is.null(dependency$trigger)){
  #     input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
  #     params <- replace_reactives(input_list, env)
  #     html <- do.call(input_list$input_info$input_type, params)
  #     return(html)
  #   }
  #   # Equals dependency case
  #   if(dependency$trigger %in% c("equals", "one_of")){
  #     if(dep_value %in% dependency$value){
  #       input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
  #       params <- replace_reactives(input_list, env)
  #       html <- do.call(input_list$input_info$input_type, params)
  #       return(html)
  #     }
  #   }
  # }
  html
}


