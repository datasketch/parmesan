
#' @export
parmesan_input_ids <- function(parmesan = NULL, section = NULL){

  if(is.null(parmesan)){
    parmesan <- parmesan_load()
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
parmesan_watch <- function(input, parmesan = NULL){
  if(is.null(parmesan)) parmesan <- parmesan_load()
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





