


#' @export
parmesan_config <- function(inputs_params = NULL, inputs_layout = NULL, config_path = "parmesan"){
  # TODO list all possible inputs for a given package, e.g. shiny, shinyWidgets, etc.
  # Modify list with all possible inputs
  if(!dir.exists(config_path)){
    stop("Parmesan folder not found")
  }
  if(!is.list(inputs_params)){
    inputs <- yaml::read_yaml(file.path(config_path, "inputs.yaml"))
  }

  if(is.null(inputs_layout)){
    if(file.exists(file.path(config_path, "/layout.yaml"))){
      layout <- yaml::read_yaml(file.path(config_path, "layout.yaml"))
    }else{
      layout <- list(section = names(inputs))
    }
  }
  # else{
  #   if(is.list(inputs_layout)){
  #   }
  # }

  ### TODO add validation for layout

  list(layout = layout, inputs = inputs)
}




