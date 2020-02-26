#' @importFrom shiny tags
#' @export
parmesan_render_ui <- function(sections = NULL, parmesan = NULL, config_path = NULL,
                               container_section = NULL,
                               container_title = NULL,
                               container_element = NULL,
                               lang = NULL,
                               input = NULL,
                               env = env){

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
    if (is.null(input_list$depends_on)) {
      # Has no dependencies
      message("No dependencies: ", input_list$name)
      input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
      html <- div(id = paste0("shn-", input$input_info$input_params$inputId),
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
      if(!is.null(input[[dependency$name]])){
        dep_value <- input[[dependency$name]]
      } else {
        dep_value <- do.call(dependency$name, list()
                             ,
                             envir = env
                             # envir = parent.frame()
                             # envir = parent.env(parent.frame())
                             # envir = parent.env(parent.env(parent.frame()))
                             # envir = parent.env(parent.env(parent.env(parent.frame())))
                             # envir = parent.env(parent.env(parent.env(parent.env(parent.frame()))))
                             # envir = parent.env(parent.env(parent.env(parent.env(parent.env(parent.frame())))))
                             # envir = parent.env(parent.env(parent.env(parent.env(parent.env(parent.env(parent.frame()))))))
                             # envir = globalenv()
                             # envir = pryr::where(dependency$name)
                             #envir = parent.env(pryr::where(dependency$name))
        )
      }
      message("Dep Value: ", dep_value)
      str(dep_value)
      # Single dependency case
      if(is.null(dependency$trigger)){
        input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
        params <-  input_list$input_info$input_params
        message("params")
        str(params)
        par <- names(Filter(function(x) x == dependency$value, params))
        str(params[[par]])
        params[[par]] <- dep_value
        str(params)
        html <- do.call(input_list$input_info$input_type, params)
        return(html)
      }
      # Equals dependency case
      if(dependency$trigger == "equals"){
        if(dep_value == dependency$value){
          input_list$input_info$input_params$label <- render_label(input_list$input_info$input_params$label)
          params <-  input_list$input_info$input_params
          html <- do.call(input_list$input_info$input_type, params)
          return(html)
        }
      }
    }
  } else {
    return()
  }
}


available_inputs <- function(){
  c("actionButton", "sliderInput", "selectInput", "selectizeInput", "checkboxInput", "radioButtons", "numericInput")
}


