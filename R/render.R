
#' @export
render_section <- function(section = NULL,
                           parmesan = NULL,
                           container_section = NULL,
                           container_title = NULL,
                           container_element = NULL,
                           input = NULL,
                           env = parent.env(),
                           render_inputs = TRUE
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
      render_par_input(par_input, input = input, env = env)
    })
  }

  if(!is.empty(section$inputs)){
    container_section(
      container_title(id = section$id, class = 'par_section', section$label),
      rendered_inputs
    )
  }

}

# reactive <- function(...){
#   shiny::reactive(..., env = )
# }


render_par_input <- function(par_input, input, env){
  # message("\nRendering input: ", par_input$id, "\n")
  if(!par_input$input_type %in% available_inputs())
    stop(par_input$input_type, " is not a registered input")
  if(!par_input$show) return()

  # Replace any reactives
  if(input_has_reactives(par_input)){
    par_input <- replace_reactives(par_input, env = env)
  }

  # Has no dependencies
  if (!input_has_dependencies(par_input)) {
    html <- render_par_html(par_input)
    return(html)
  }
  # Show if dependency
  if(input_has_show_if(par_input)){
    if(is.null(input)) stop("Need to pass input to render_section")
    # if(is.null(validate_show_if(par_input, input))) return()
    if(validate_show_if(par_input, input)){
      html <- render_par_html(par_input)
      return(html)
    }
  }

}

#' @export
replace_reactives <- function(par_input, env){
  params <-  par_input$input_params
  pars <- names(Filter(function(x) grepl("reactive__", x), params))
  # str(pars)
  params_reactive <- lapply(pars, function(par){
    inp <- par_input$input_params[[par]]
    # if(is.null(inp)) return()
    reactive_fun <- gsub("reactive__","", inp)
    # message("reactive fun: ", reactive_fun)
    dep_value_params <- do.call(reactive_fun, list(), envir = env)
    # dep_value_params <- do.call(reactive_fun, list())
    # str(dep_value_params)
    dep_value_params
  })
  names(params_reactive) <- pars
  # params[[par]] <- dep_value_params
  params <- modifyList(params, params_reactive)
  par_input$input_params <- params
  par_input
}



validate_show_if <- function(par_input, input){
  if(is.null(par_input)) return()
  dep <- list(
    id = names(par_input$show_if),
    condition = names(par_input$show_if[[1]]),
    value = par_input$show_if[[1]][[1]]
  )
  str(dep)
  target_value <- input[[dep$id]]
  if(is.null(target_value)) return(FALSE)
  str(target_value)
  if(dep$condition == "equals" && dep$value == target_value){
    return(TRUE)
  }
  FALSE
}

render_par_html <- function(par_input){
  par_input$input_params$inputId <- par_input$id
  # str(par_input$input_params)
  do.call(par_input$input_type, par_input$input_params)
}

input_has_reactives <- function(par_input){
  any(grepl("reactive__", par_input$input_params))
}

input_has_show_if <- function(par_input){
  !is.null(par_input$show_if)
}

input_has_dependencies <- function(par_input){
  !is.null(par_input$show_if) || grepl("reactive__", names(par_input))
}

