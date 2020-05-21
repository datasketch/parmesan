


validate_input_type <- function(par_input){
  if(is.null(par_input$input_type))
    stop("Input with id: ", par_input$id, " has input_type is NULL")
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


infoTooltip <- function(par_input) {

  icn <- par_input$input_info$icon %||% "info-circle"
  id <- par_input$id
  inp <- par_input$input_type
  title <- par_input$input_params$label
  info <- par_input$input_info$text
  cn_style <- par_input$input_info$`container-style` %||% ""
  ic_style <- par_input$input_info$`icon-style` %||% ""
  tl_style <- par_input$input_info$`tooltip-style` %||% ""

  sl <- paste0(".control-label[for = '", id, "'] {width: 100%;}")
  if (inp == "actionButton")
    # sl <- paste0("#", id, "{width: 100%;}")
    sl <- ""
  if (inp %in% c("selectInput", "selectizeInput"))
    sl <- paste0(".control-label[for = '", id, "-selectized'] {width: 100%;}")
  if (inp == "checkboxInput")
    sl <- ".checkbox > label {width: 100%;}"

  style <- paste0("
  .info-tool {
  display: inline-flex;
  }
  .tooltip-inf {
  cursor: pointer;
  position: relative;
  margin-left: 3px;
  }
  .tooltip-inf .tooltiptext {
  background: #eee;
  display: inline-block;
  font-size: 13px;
  left: 20px;
  max-width: 200px;
  padding: 7px 10px;
  position: absolute;
  top: -12px;
  visibility: hidden;
  z-index: 100;
  }
  .tooltip-inf:hover .tooltiptext {
  visibility: visible;
  } ", sl)

  tagList(tags$head(tags$style(style)),
          HTML(paste0('<div style = "display: inline-flex; align-items: baseline; width: 100%;', cn_style, '">',
                      title,
                      '<div class = "info-tool"> <div class="tooltip-inf" style = "', ic_style,'">',
                      shiny::icon(icn),
                      '<span class = "tooltiptext" style = "', tl_style, '">',
                      info,
                      '</span></div></div></div>')))
}

