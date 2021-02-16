render_par_input <- function(par_input, input, env = parent.frame(),
                             debug = FALSE){
  # message("\nRendering input: ", par_input$id, "\n")

  if(!par_input$show) return()

  # Replace any reactives in param values
  if(input_has_reactive_param_values(par_input)){
    par_input <- replace_reactive_param_values(par_input, env = env)
  }

  # Replace any reactives tooltip
  if(input_has_reactive_tooltip_text(par_input)){
    message("\n\nHAS REACTIVE TOOLTIP")
    str(par_input)
    #par_input <- replace_reactive_param_values(par_input, env = env)
    str(replace_reactive_tooltip_text(par_input, env = env))
    par_input <- replace_reactive_tooltip_text(par_input, env = env)
  }

  # Has no conditionals
  if (!input_has_show_if(par_input)) {
    html <- render_par_html(par_input)
    return(html)
  }
  # Show if dependency
  if(input_has_show_if(par_input)){
    if(is.null(input)) stop("Need to pass input to render_section")
    if(validate_show_if(par_input, input, env, debug = debug)){
      html <- render_par_html(par_input)
      #html <- render_par_html(par_input, env = env, debug = debug)
      return(html)
    }
  }

}

replace_reactive_param_values <- function(par_input, env = parent.frame()){
  params <-  par_input$input_params
  pars <- names(Filter(function(x) grepl("\\(\\)", x), params))
  params_reactive <- lapply(pars, function(par){
    inp <- par_input$input_params[[par]]
    dep_value_params <- do.call(remove_parenthesis(inp), list(), envir = env)
    dep_value_params
  })
  names(params_reactive) <- pars
  params <- modifyList(params, params_reactive, keep.null = TRUE)
  par_input$input_params <- params
  par_input
}

replace_reactive_tooltip_text <- function(par_input, env = parent.frame()){
  text <-  par_input$input_info$text
  text <- do.call(remove_parenthesis(text), list(), envir = env)
  par_input$input_info$text <- text
  par_input
}



validate_show_if <- function(par_input, input, env, debug = FALSE){
  if(is.null(par_input)) return()

  if(debug) message("\nRendering: ", par_input$id)
  condition_type <- names(par_input)[grep("show_",names(par_input))]

  conditions <- lapply(seq_along(par_input$show_if), function(i){
    condition <- names(par_input$show_if[[i]])
    value1 <- names(par_input$show_if)[[i]]
    value2 <- par_input$show_if[[i]][[1]]
    value2ini <- value1ini <- NULL
    if(is_shiny_input(value1, input)){
      value1ini <- value1
      value1 <- input[[value1]]
    }
    if(is_reactive_string(value1)){
      value1ini <- value1
      value1 <- do.call(remove_parenthesis(value1), list(), envir = env)
    }
    if(is_shiny_input(value2, input)){
      value2ini <- value2
      value2 <- input[[value2]]
    }
    if(is_reactive_string(value1)){
      value2ini <- value2
      value2 <- do.call(reactive_fun, list(), envir = env)
    }
    if(debug){
      message(
        "- value1: ", value1ini, " ", paste0(value1, collapse = ", "),
        "\n  condition: ", condition,
        "\n  value2: ", value2ini, " ", paste0(value2, collapse = ", ")
      )
    }
    list(value1 = value1, condition = condition, value2 = value2)
  })

  pass_conditions <- unlist(lapply(conditions, function(x)
    eval_conditions(x$value1, x$condition, x$value2)))

  if(condition_type %in% c("show_if", "show_if_all"))
    return(all(pass_conditions))
  if(condition_type == "show_if_any")
    return(any(pass_conditions))
}



render_par_html <- function(par_input) {

  inputtype <- par_input$input_type
  input_type_with_ns <- input_namespace(inputtype)

  par_input$input_params$inputId <- par_input$id
  if (!is.null(par_input$input_info)) {
    par_input$input_params$label <- parmesan:::infoTooltip(par_input)
    return(do.call(getfun(input_type_with_ns), par_input$input_params))
  }
  return(do.call(getfun(input_type_with_ns), par_input$input_params))
}


