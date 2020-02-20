#' @importFrom shiny tags
#' @export
parmesan_render_ui <- function(sections = NULL, parmesan = NULL, config_path = NULL,
                               container_section = NULL,
                               container_title = NULL,
                               container_element = NULL,
                               lang = NULL){

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

  children <- names(inputs_params)[grep("child_info", inputs_params)]
  lapply(sections, function(section) {
    container_section(
      container_title(id = section, class = 'style_section',
                      render_label(section)),
      lapply(inputs_layout[[section]], function(param_i) {
        p_inf <- inputs_params[[param_i]]
        if (is.null(p_inf)) return()
        if(!p_inf$input_info$input_type %in% available_inputs())
          stop(p_inf$input_info$input_type, " is not a registered input")
        if (p_inf$show) {
          if (param_i %in% children) {
            p_inf$input_info$input_params$label <- render_label(p_inf$input_info$input_params$label)
            div(id = paste0("shn-", p_inf$input_info$input_params$inputId),
                do.call(p_inf$input_info$input_type, p_inf$input_info$input_params))
          } else {
            p_inf$input_info$input_params$label <- render_label(p_inf$input_info$input_params$label)
            do.call(p_inf$input_info$input_type, p_inf$input_info$input_params)
          }
        } else {
          return()
        }
      })
    )
  })

}


available_inputs <- function(){
  c("actionButton", "sliderInput", "selectInput", "selectizeInput", "checkboxInput", "radioButtons", "numericInput")
}


