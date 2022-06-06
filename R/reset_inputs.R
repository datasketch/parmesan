#'@export
resetButtonUI <- function(id = "reset_button", icon_name = "trash" ,class = "btn action-button") {
  ns <- shiny::NS(id)
  shiny::actionButton(inputId = ns("resetButton"),
                      label = " ",
                      icon = icon(icon_name),
                      class = class)
}

#' @export
resetButtonServer <- function(session, input, id, id_reset = "all") {

  ns <- session$ns
  buttonId <- paste0(id, "-", ns("resetButton"))

  observeEvent(input[[buttonId]], {

    df_inputs <- parmesan:::initial_inputs_namespace(parmesan_inputs())
    print(df_inputs)
    if (!("all" %in% id_reset)) {
      if (sum(id_reset %in% df_inputs$id) == 0) stop("input not found in parmesan list")
      df_inputs <- df_inputs %>% dplyr::filter(id %in% id_reset)
    }

    parmesan:::updateInput_function(session, df_inputs = df_inputs)

  })


}
