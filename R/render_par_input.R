render_par_input <- function(par_input,
                             parent = NULL,
                             debug = FALSE){

  if(!par_input$show) return()

  html <- render_par_html(par_input = par_input, parent = parent)
  return(html)
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
      value1 <- evaluate_input(x = value1, r = r)
    }
    if(is_reactive_string(value1)){
      value1ini <- value1
      value1 <- evaluate_reactive(x = value1, env = env, r = r)
    }
    if(is_shiny_input(x = value2, input = input, r = r)){
      value2ini <- value2
      value2 <- evaluate_input(x = value2, r = r)
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

  par_input$input_params$inputId <- par_input_id
  if (!is.null(par_input$input_info)) {
    par_input$input_params$label <- parmesan:::infoTooltip(par_input)
    return(do.call(getfun(input_type_with_ns), par_input$input_params))
  }
  return(do.call(getfun(input_type_with_ns), par_input$input_params))
}


