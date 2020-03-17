#' @importFrom shiny tags
#' @export
parmesan_render_ui <- function(sections = NULL, parmesan = NULL,
                               config_path = "parmesan",
                               container_section = NULL,
                               container_title = NULL,
                               container_element = NULL,
                               lang = NULL,
                               input = NULL,
                               env = parent.frame()){

  if(is.null(parmesan)){
    parmesan <- parmesan_config(config_path = config_path)
  }
  inputs_layout <- parmesan$layout
  inputs_params <- parmesan$inputs

  if(is.null(sections)){
    sections <- names(inputs_layout)
  }else{
    if(!all(sections %in% names(inputs_layout))){
      stop("Sections not defined in inputs_layout")
    }
  }
  if(is.null(container_section)){
    container_section <- shiny::tags$div
  }
  if(is.null(container_title)){
    container_title <- shiny::tags$div
  }
  if(is.null(container_section)){
    container_element <- shiny::tags$div
  }

  if(!is.null(lang)){
    if(shiny::is.reactive(lang)){
      lang <- lang()
    }
    render_label <- function(x) i_(x, lang)
  } else{
    render_label <- function(x) x
  }

  #children <- names(inputs_params)[grep("child_info", inputs_params)]
  lapply(sections, function(section) {
    container_section(
      container_title(id = section, class = 'style_section',
                      render_label(section)),
      lapply(inputs_layout[[section]], function(input_id) {
        input_list <- inputs_params[[input_id]]
        input_list$name <- input_id
        render_input(input_list, render_label, input = input, env = env)
      })
    )
  })

}


render_input <- function(input_list, render_label, input, env){
  message("\nRendering input: ", input_list$name, "\n")
  if (is.null(input_list)) return()
  # if(shiny::is.reactive(lang)){
  #   lang <- lang()
  # }
  if(!input_list$input_info$input_type %in% available_inputs())
    stop(input$input_info$input_type, " is not a registered input")
  if (input_list$show) {
    input_list$input_info$input_params$inputId <- input_list$name
    if (is.null(input_list$depends_on)) {
      # Has no dependencies
      message("No dependencies: ", input_list$name)
      input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
      html <- div(id = paste0("shn-", input_list$name),
                  do.call(input_list$input_info$input_type, input_list$input_info$input_params)
      )
    } else {
      # Has dependencies
      dependency <- list(
        name = names(input_list$depends_on) %||% input_list$depends_on[[1]][[1]],
        trigger = names(input_list$depends_on[[1]]),
        value = input_list$depends_on[[1]][[1]]
      )
      str(dependency)
      message("input[[",dependency$name,"]] is: ", input[[dependency$name]])
      str(input[[dependency$name]])
      if(!is.null(input[[dependency$name]])){
        # Dependency on input
        message("Dependency on input: ", dependency$name)
        dep_value <- input[[dependency$name]]
      } else {
        message("Dependency on reactive: ", dependency$name)
        reactive_fun <- gsub("reactive__","", dependency$name)
        dep_value <- do.call(reactive_fun, list(), envir = env)
      }
      message("Dep Value: ", dep_value)
      str(dep_value)
      # Single dependency case
      if(is.null(dependency$trigger)){
        input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
        params <- replace_reactives(input_list, env)
        # message("params")
        # str(params)
        # pars <- names(Filter(function(x) grepl("reactive__", x), params))
        # message("pars")
        # str(pars)
        # params_reactive <- lapply(pars, function(par){
        #   reactive_fun <- gsub("reactive__","", input_list$input_info$input_params[[par]])
        #   message("reactive fun: ", reactive_fun)
        #   dep_value_params <- do.call(reactive_fun, list(), envir = env)
        #   str(dep_value_params)
        #   dep_value_params
        # })
        # names(params_reactive) <- pars
        # # params[[par]] <- dep_value_params
        # params <- modifyList(params, params_reactive)
        # message("params reactive")
        # str(params_reactive)
        # str(params)
        html <- do.call(input_list$input_info$input_type, params)
        return(html)
      }
      # Equals dependency case
      if(dependency$trigger %in% c("equals", "one_of")){
        if(dep_value %in% dependency$value){
          input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
          params <- replace_reactives(input_list, env)
          html <- do.call(input_list$input_info$input_type, params)
          return(html)
        }
      }
    }
  } else {
    return()
  }
}


replace_reactives <- function(input_list, env){
  params <-  input_list$input_info$input_params
  pars <- names(Filter(function(x) grepl("reactive__", x), params))
  message("pars")
  str(pars)
  params_reactive <- lapply(pars, function(par){
    reactive_fun <- gsub("reactive__","", input_list$input_info$input_params[[par]])
    message("reactive fun: ", reactive_fun)
    dep_value_params <- do.call(reactive_fun, list(), envir = env)
    str(dep_value_params)
    dep_value_params
  })
  names(params_reactive) <- pars
  # params[[par]] <- dep_value_params
  modifyList(params, params_reactive)
}


available_inputs <- function(){
  l <- yaml::read_yaml(system.file("available_inputs.yaml", package = "parmesan"))
  unlist(unname(lapply(l, names)))
}


