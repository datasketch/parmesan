
available_inputs <- function(){
  l <- load_available_inputs()
  unlist(unname(lapply(l, names)))
}

load_available_inputs <- function(){
  yaml::read_yaml(system.file("available_inputs.yaml", package = "parmesan"))
}



