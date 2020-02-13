


#' @export
parmesan_config <- function(inputs_params = NULL, inputs_layout = NULL, config_path = "parmesan"){
  # TODO list all possible inputs for a given package, e.g. shiny, shinyWidgets, etc.
  # Modify list with all possible inputs
  if(!is.list(inputs_params)){
    inputs_params <- yaml::read_yaml(file.path(config_path, "params.yaml"))
  }

  if(is.null(inputs_layout)){
    if(file.exists(file.path(config_path, "/layout.yaml"))){
      inputs_layout <- yaml::read_yaml(file.path(config_path, "/layout.yaml"))
    }else{
      inputs_layout <- list(section = names(inputs_params))
    }
  }
  # else{
  #   if(is.list(inputs_layout)){
  #   }
  # }

  ### TODO add validation for layout

  list(layout = inputs_layout, params = inputs_params)
}

parmesan_input_ids <- function(section = NULL, config_path = "parmesan"){
  parmesan_config <- parmesan_config(config_path = config_path)
  if(is.null(section)){
    sections <- names(parmesan_config$layout)
  }else{
    if(!all(sections %in% names(inputs_layout))){
      stop("Sections not defined in inputs_layout")
    }
  }
  unlist(unname(parmesan_config$layout))
}
