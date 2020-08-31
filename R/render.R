
#' @export
render_section <- function(section = NULL,
                           parmesan = NULL,
                           container_section = NULL,
                           container_title = NULL,
                           container_element = NULL,
                           input = NULL,
                           env = parent.frame(),
                           render_inputs = TRUE,
                           debug = FALSE
                           ){

  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }else if (shiny::is.reactive(parmesan)){
    parmesan <- parmesan()
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

  rendered_inputs <- NULL
  if(render_inputs){
    rendered_inputs <- lapply(section$inputs, function(par_input) {
      render_par_input(par_input, input = input, env = env, debug = debug)

    })
  }

  if(!is.empty(section$inputs)){
    container_section(
      container_title(id = section$id, class = 'par_section',
                      div(class = "style_section",
                          style = "font-size: medium;font-weight: bolder;",
                           section$label)),
      rendered_inputs,
      shiny::tags$script("Shiny.onInputChange('parmesan_updated',+new Date);")
    )
  }

}
