
available_inputs <- function(){
  l <- yaml::read_yaml(system.file("available_inputs.yaml", package = "parmesan"))
  unlist(unname(lapply(l, names)))
}
