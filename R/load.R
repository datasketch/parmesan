

#' @export
parmesan_load <- function(path = "parmesan", inputs_only = FALSE,
                          debug = FALSE, presets = NULL){

  if(!dir.exists(path)){
    stop("Parmesan folder not found")
  }

  if(shiny::is.reactive(presets)){
    presets <- presets()
  }

  inputs <- yaml::read_yaml(file.path(path, "inputs.yaml"))
  if(inputs_only) return(inputs)
  if(file.exists(file.path(path, "layout.yaml"))){
    layout <- yaml::read_yaml(file.path(path, "layout.yaml"))
  }else{
    layout <- list(section = names(inputs))
  }
  inputs_in_layout <- unlist(lapply(layout, function(x){x$inputs}))
  if(!all(inputs_in_layout %in% names(inputs)))
    stop("inputs in layout.yaml not defined in inputs.yaml: ",
            paste0(inputs_in_layout[!inputs_in_layout %in% names(inputs)], collapse = ", "))
  if(!all(names(inputs) %in% inputs_in_layout))
    warning("inputs in inputs.yaml not defined in layout.yaml: ",
         paste0(names(inputs)[! names(inputs) %in% inputs_in_layout], collapse = ", "))
  if(!is.empty(which_repeated(names(inputs))))
     stop("Repeated inputs defined in inputs.yaml")
  if(!is.empty(which_repeated(inputs_in_layout)))
     stop("Repeated inputs defined in layout.yaml")

  parmesan <- layout
  section_ids <- names(layout)
  parmesan <- lapply(seq_along(layout), function(j){
    if(debug)
      message("Rendering layout section", "(", j,"): ", section_ids[j])
    # x <- layout[[2]]
    x <- list(id = section_ids[j])
    x <- c(x, layout[[j]])
    if(debug)
      message("with inputs: ", paste0(x$inputs, collapse = ", "))
    which_null_inputs <- unlist(lapply(x$inputs, is.null))
    if(any(which_null_inputs))
      stop("Layout section: ", section_ids[j], ". Input number ",
           paste0(which(which_null_inputs), collapse = ", "), " is NULL")
    inputs <- inputs[x$inputs]
    x$inputs <- lapply(seq_along(inputs), function(i){
      if(debug)
        message("  input id : ", x$inputs[i])
      input_id <- x$inputs[i]
      input <- list(id = input_id)
      input <- c(input, inputs[[i]])
      if(!is.null(presets)){
        input_preset <- presets[[input_id]] %||% list()
        input$input_params <- modifyList(input$input_params, input_preset)
      }
      class(input) <- "parmesan_input"
      input
    })
    lapply(x$inputs, validate_input_type)
    x
  })
  names(parmesan) <- section_ids
  class(parmesan) <- "parmesan"
  if(debug) message("Rendered layout OK.")
  parmesan
}


which_repeated <- function(x){
  names(which(table(x) > 1))
}


