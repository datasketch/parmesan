
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
                      span(class = "section-label",
                           style = "font-size: large;/*! font-weight: bolder; *//*! font-style: revert; */color: #5050a9;text-decoration:underline;",
                           section$label)),
      rendered_inputs,
      shiny::tags$script("Shiny.onInputChange('parmesan_updated',+new Date);")
    )
  }

}
