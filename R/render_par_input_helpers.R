


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

input_has_reactive_tooltip_text <- function(par_input){
  any(grepl("\\(\\)", par_input$input_info$text))
}

remove_parenthesis <- function(x){
  gsub("\\(\\)","",x)
}

is_reactive_string <- function(x){
  any(grepl("\\(\\)", x))
}

is_shiny_input <- function(x, input, r = NULL){
  if(shiny::is.reactive(x)) return(FALSE)
  if(!is.character(x)) return(FALSE)
  # For multiple values in conditional inputs
  # Doesn't work yet when conditionals are vectors or reactives
  if(length(x) > 1) return(FALSE)

  validate <- !is.null(input[[x]])
  if(!is.null(r)){
    validate <- !is.null(r[[x]])
  }
 validate
}



input_has_show_if <- function(par_input){
  # !is.null(par_input$show_if) #|| grepl("reactive__", names(par_input))
  any(grepl("show_if", names(par_input)))
}


# infoTooltip <- function(par_input, debug = FALSE) {
#
#   if(debug){
#     message("in tooltip")
#     str(par_input)
#   }
#
#   icn <- par_input$input_info$icon %||% "info-circle"
#   id <- par_input$id
#   inp <- par_input$input_type
#   title <- par_input$input_params$label
#   info <- par_input$input_info$text
#
#   ic_a <- par_input$input_info$`icon-align` %||% "left"
#   sl0 <- paste0(".control-label[for = '", id, "-selectized'] {position: relative;}")
#   sl1 <- ""
#   js <- "flex-start"
#   if (ic_a == "right") {
#     sl0 <- paste0(".control-label[for = '", id, "'] {width: 100%; position: relative;}")
#     if (inp == "actionButton")
#       # sl <- paste0("#", id, "{width: 100%;}")
#       sl0 <- ""
#     if (inp %in% c("selectInput", "selectizeInput"))
#       sl0 <- paste0(".control-label[for = '", id, "-selectized'] {width: 100%; position: relative;}")
#     if (inp == "checkboxInput")
#       sl0 <- HTML(".checkbox > label {width: 100%;}")
#
#     js <- "space-between;"
#   }
#   if (inp == "checkboxInput") {
#     sl1 <- HTML(paste0(sl1, ".checkbox > label > span {display: inline-block;}"))
#   }
#
#   sl2 <- "
#   .info-tool {
#   display: inline-flex;
#   }
#   .tooltip-inf {
#   cursor: pointer;
#   position: relative;
#   margin-left: 3px;
#   }
#   .tooltip-inf .tooltiptext {
#   background: #eee;
#   display: inline-block;
#   font-size: 13px;
#   left: 0;
#   max-width: 200px;
#   padding: 7px 10px;
#   position: absolute;
#   top: calc(100% + 5px);
#   visibility: hidden;
#   z-index: 100;
#   }
#   .info-tool:hover .tooltiptext {
#   visibility: visible;
#   } "
#
#   tagList(shiny::singleton(tags$head(tags$style(sl0))),
#           shiny::singleton(tags$head(tags$style(sl1))),
#           shiny::singleton(tags$head(tags$style(sl2))),
#           HTML(paste0('<div style = "display: inline-flex; align-items: baseline; width: 100%; justify-content: ', js, '">',
#                       title,
#                       '<div class = "info-tool"> <div class="tooltip-inf">',
#                       shiny::icon(icn),
#                       '<span class = "tooltiptext" style = "font-weight: normal;">',
#                       info,
#                       '</span></div></div></div>')))
# }


infoTooltip <- function(par_input, debug = FALSE) {

  if(debug){
    message("in tooltip")
    str(par_input)
  }

  icn <- par_input$input_info$icon %||% "info-circle"
  id <- par_input$id
  inp <- par_input$input_type
  title <- par_input$input_params$label
  info <- par_input$input_info$text

  sl0 <- "
  .tooltip0 {
    align-items: center;
    display: flex;
  }

  .tooltip0-slot {
    margin-left: 4px;
  }

  .tooltip0-icon {
    color: #8097a4;
    cursor: pointer;
  }

  .tooltip0-content {
    background: #ffffff;
    border: 1px solid #e6eaed;
    box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1);
    color: #435b69;
    display: none;
    font-family: 'IBM Plex Sans', sans-serif;
    font-size: 0.75rem
    font-size: 13px;
    max-width: 250px;
    min-width: 180px;
    padding: 1rem;
    position: absolute;
  }

  .tooltip0-slot:hover .tooltip0-content {
    display: block;
    z-index: 10;
  }
  "
  tagList(shiny::singleton(tags$head(tags$style(sl0))),
          HTML(paste0('<div class="tooltip0">',
                      '<span>', title, '</span>',
                      '<div class="tooltip0-slot"><span class="tooltip0-icon">',
                      shiny::icon(icn),
                      '</span><div class="tooltip0-content" style = "font-weight: normal;">',
                      info,
                      '</div></div></div>')))

}
