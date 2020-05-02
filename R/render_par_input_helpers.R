


validate_input_type <- function(par_input){
  if(!par_input$input_type %in% available_inputs())
    stop(par_input$input_type, " is not a registered input. ",
         "Try using one of:\n", paste0(available_inputs(), collapse = ", "))
}

input_has_reactive_param_values <- function(par_input){
  any(grepl("\\(\\)", par_input$input_params))
}

remove_parenthesis <- function(x){
  gsub("\\(\\)","",x)
}

is_reactive <- function(x){
  any(grepl("\\(\\)", x))
}
is_shiny_input <- function(x, input){
  if(!is.character(x)) return(FALSE)
  !is.null(input[[x]])
}



input_has_show_if <- function(par_input){
  # !is.null(par_input$show_if) #|| grepl("reactive__", names(par_input))
  any(grepl("show_if", names(par_input)))
}



