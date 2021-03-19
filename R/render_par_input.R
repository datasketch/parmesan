render_par_input <- function(par_input,
                             input = NULL,
                             env = NULL,
                             parent = NULL,
                             r = NULL,
                             debug = FALSE){

  if(!par_input$show) return()

  # Replace any reactives in param values
  if(input_has_reactive_param_values(par_input)){
    par_input <- replace_reactive_param_values(par_input, env = env, parent = parent, r = r)
  }

  # Replace any reactives tooltip
  if(input_has_reactive_tooltip_text(par_input)){
    message("\n\nHAS REACTIVE TOOLTIP")
    str(par_input)
    #par_input <- replace_reactive_param_values(par_input, env = env)
    # str(replace_reactive_tooltip_text(par_input, r = r, env = env))
    par_input <- replace_reactive_tooltip_text(par_input, r = r, env = env)
  }

  # Has no conditionals
  if (!input_has_show_if(par_input)) {
    html <- render_par_html(par_input, parent)
    return(html)
  }
  # Show if dependency
  if(input_has_show_if(par_input)){
    if(is.null(input)) stop("Need to pass input to render_section")
    if(validate_show_if(par_input = par_input, input = input, env = env, parent = parent, r = r, debug = debug)){
      html <- render_par_html(par_input, parent)
      #html <- render_par_html(par_input, env = env, debug = debug)
      return(html)
    }
  }

}

replace_reactive_param_values <- function(par_input, env = parent.frame(), parent = NULL, r = NULL){
  params <-  par_input$input_params
  pars <- names(Filter(function(x) grepl("\\(\\)", x), params))

  params_reactive <- lapply(pars, function(par){
    inp <- par_input$input_params[[par]]

    if(is.null(r)){
      dep_value_params <- do.call(remove_parenthesis(inp), list(), envir = env)
    } else {
      dep_value_params <- do.call(r[[remove_parenthesis(inp)]], list())
    }
    dep_value_params
  })
  names(params_reactive) <- pars
  params <- modifyList(params, params_reactive, keep.null = TRUE)
  par_input$input_params <- params
  par_input
}

replace_reactive_tooltip_text <- function(par_input, r = NULL, env = parent.frame()){
  text <-  par_input$input_info$text

  if(is.null(r)){
    text <- do.call(remove_parenthesis(text), list(), envir = env)
  } else {
    text <- do.call(r[[remove_parenthesis(text)]], list())
  }

  par_input$input_info$text <- text
  par_input
}


validate_show_if <- function(par_input, input, env, parent, r = NULL, debug = FALSE){
  if(is.null(par_input)) return()

  ns <- parent$ns

  if(debug) message("\nRendering: ", par_input$id)
  condition_type <- names(par_input)[grep("show_",names(par_input))]

  conditions <- lapply(seq_along(par_input$show_if), function(i){
    condition <- names(par_input$show_if[[i]])
    value1 <- names(par_input$show_if)[[i]]
    value2 <- par_input$show_if[[i]][[1]]

    value2ini <- value1ini <- NULL
    if(is_shiny_input(x = value1, input = input, r = r)){
      value1ini <- value1
      value1 <- evaluate_input(x = value1, input = input, r = r)
    }
    if(is_reactive_string(value1)){
      value1ini <- value1

      if(is.null(r)){
        value1 <- do.call(remove_parenthesis(value1), list(), envir = env)
      } else {
        value1 <- do.call(r[[remove_parenthesis(value1)]], list())
      }
    }
    if(is_shiny_input(x = value2, input = input, r = r)){
      value2ini <- value2
      value2 <- evaluate_input(x = value2, input = input, r = r)
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


render_par_html <- function(par_input, parent = NULL) {

  par_input_id <- par_input$id
  if(!is.null(parent)){
    ns <- parent$ns
    par_input_id <- ns(par_input$id)
  }

  input_type <- par_input$input_type
  input_type_with_ns <- input_namespace(input_type)

  # If selected not in choices, use first choices option as selected
  choices <- par_input$input_params$choices
  selected <- par_input$input_params$selected
  if(!any(is.null(choices), is.null(selected))){
    if(!selected %in% choices){
      warning("Value ",selected, " not in choices for ",par_input$input_params$label,". Using first value of choices vector instead.")
      selected <- choices[1]
      par_input$input_params$selected <- selected
    }
  }

  par_input$input_params$inputId <- par_input_id
  if (!is.null(par_input$input_info)) {
    par_input$input_params$label <- parmesan:::infoTooltip(par_input)
    return(do.call(getfun(input_type_with_ns), par_input$input_params))
  }
  return(do.call(getfun(input_type_with_ns), par_input$input_params))
}


