
available_inputs <- function(){
  l <- load_available_inputs()
  unlist(unname(lapply(l, names)))
}

load_available_inputs <- function(){
  yaml::read_yaml(system.file("available_inputs.yaml", package = "parmesan"))
}

lookup_input_namespace <- function(){
  inputs <- load_available_inputs()
  inputs_packages <- data.frame(names = names(unlist(load_available_inputs(), recursive = FALSE))) %>%
    tidyr::separate(names, c("package", "input")) %>%
    dplyr::mutate(ns = paste0(package, "::", input))
  inputs_lookup <- inputs_packages %>% dplyr::pull(ns)
  names(inputs_lookup) <- inputs_packages %>% dplyr::pull(input)
  inputs_lookup
}

lookup_updateInput_namespace <- function(){
  inputs <- load_available_inputs()
  inputs_packages <- data.frame(names = names(unlist(load_available_inputs(), recursive = FALSE))) %>%
    tidyr::separate(names, c("package", "input")) %>%
    dplyr::mutate(ns = paste0(package, "::update", firstup(input)))
  inputs_lookup <- inputs_packages %>% dplyr::pull(ns)
  names(inputs_lookup) <- inputs_packages %>% dplyr::pull(input)
  inputs_lookup
}

input_namespace <- function(input){
  as.character(lookup_input_namespace()[input])
}

updateInput_namespace <- function(input){
  as.character(lookup_updateInput_namespace()[input])
}

