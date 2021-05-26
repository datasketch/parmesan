
#' @export
parmesan_input_ids <- function(parmesan = NULL, section = NULL){

  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }else if (shiny::is.reactive(parmesan)){
    parmesan <- parmesan()
  }
  if(!is.null(section)){
    parmesan <- parmesan[[section]]
  }

  unname(unlist(lapply(parmesan, function(x){
    lapply(x$inputs,function(y){
      y$id
    })
  })))
}

#' @export
parmesan_inputs <- function(parmesan = NULL, section = NULL){
  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }
  if(!is.null(section)){
    parmesan <- parmesan[[section]]
  }
  inputs <- unname(unlist(lapply(parmesan, function(x){
    x$inputs
  }),recursive = FALSE))
  inputs
}

#' @export
parmesan_input_values <- function(parmesan = NULL, section = NULL){
  inputs <- parmesan_inputs(parmesan = parmesan, section = section)
  unlist(lapply(inputs,function(x){
    l <- list(x$input_params$value %||%
                x$input_params$selected %||%
                x$input_params$color %||%
                x$input_params$colors)
    setNames(l, x$id)
  }),recursive = FALSE)
}



#' @export
parmesan_watch <- function(input, parmesan = NULL){
  #if(is.null(parmesan)) parmesan <- parmesan_load()
  if(is.null(parmesan)) return()
  parmesan_inputs <- reactiveValues()
  # parmesan_inputs$inputs <- parmesan_input_values(parmesan)
  # parmesan_env <- new.env()
  observe({
    lapply(parmesan_input_ids(parmesan = parmesan), function(i){
      parmesan_inputs[[i]] <- input[[i]]
      parmesan_inputs
    })
  })
  reactive(reactiveValuesToList(parmesan_inputs))
}


#' Show all reactive elements of parmesan object
#'
#' Returns all reactive elements from parmesan object,
#' including parameters, conditions (in show_if statements),
#' and reactive info tooltip text.
#'
#' @param parmesan Parmesan object
#'
#' @return Character vector of all reactive strings.
#' @export
parmesan_reactives <- function(parmesan){

  purrr::map(parmesan, function(section){
    purrr::map(section$inputs, function(par_input){

      # Include params in check
      check <- par_input$input_params

      # Include conditional values in check
      if("show_if" %in% names(par_input)){
        conditional_values <- get_conditional_values(par_input)
        check <- c(check, conditional_values)
      }

      # Include tooltip text in check
      if(!is.null(par_input$input_info$text)){
        check <- c(check, par_input$input_info$text)
      }

      # Filter for brackets to extract reactive strings
      Filter(function(x) grepl("\\(\\)", x), check)

    }) %>% unlist(use.names = FALSE)
  }) %>% unlist(use.names = FALSE)
}


get_conditional_values <- function(par_input){

  conditional_values <- purrr::map(seq_along(par_input$show_if), function(i){
    list(names(par_input$show_if)[[i]],
         par_input$show_if[[i]][[1]])})

  purrr::flatten(conditional_values)
}








