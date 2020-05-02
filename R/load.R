
parmesan_load <- function(path = "parmesan", inputs_only = FALSE){

  if(!dir.exists(path)){
    stop("Parmesan folder not found")
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
    # x <- layout[[2]]
    x <- list(id = section_ids[j])
    x <- c(x, layout[[j]])
    inputs <- inputs[x$inputs]
    x$inputs <- lapply(seq_along(inputs), function(i){
      input <- list(id = x$inputs[i])
      input <- c(input, inputs[[i]])
      class(input) <- "parmesan_input"
      input
    })
    x
  })
  names(parmesan) <- section_ids
  class(parmesan) <- "parmesan"
  parmesan
}


which_repeated <- function(x){
  names(which(table(x) > 1))
}


