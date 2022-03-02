
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


initial_inputs_namespace <- function(parmesanInputs = NULL) {
  if (is.null(parmesanInputs)) return()

  df_parmesan <-
    purrr::map(seq_along(parmesanInputs), function(i) {
      data.frame(id = parmesanInputs[[i]]$id,
                 input_type = parmesanInputs[[i]]$input_type,
                 update_param = parmesanInputs[[i]]$update_param)
    }) %>% dplyr::bind_rows()

  df_parmesan

}

updateInput_function <- function(session, df_inputs) {
  if (is.null(df_inputs)) return()

  purrr::map(unique(df_inputs$id), function(id_inputs){
    input_filter <- df_inputs %>% dplyr::filter(id %in% id_inputs)
    input_type <- input_filter %>% .$input_type
    update_param <- input_filter %>% .$update_param
    update_value <- parmesan::parmesan_input_values()[[id_inputs]]
    update_list <- list(update_value)
    names(update_list) <- c(update_param)
    update_list$session <- session
    update_list$inputId <- id_inputs
    update_function <- updateInput_namespace(input_type)
    do.call(eval(parse(text = update_function)), args = update_list)
  })

}

