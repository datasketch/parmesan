
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


#' @export
parmesan_reactives <- function(parmesan){
  purrr::map(parmesan, function(section){
    purrr::map(section$inputs, function(par_input){
      params <- par_input$input_params
      Filter(function(x) grepl("\\(\\)", x), params)
    }) %>% unlist(use.names = FALSE)
  }) %>% unlist(use.names = FALSE)
}





