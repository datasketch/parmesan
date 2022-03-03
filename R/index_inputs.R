index_inputs <- function(session, input, parmesan = NULL) {

  parmesan <- parmesan
  initial_values <- parmesan::parmesan_input_values(parmesan = parmesan)

   l <- purrr:::map(names(initial_values), function(i) {
      iv <- initial_values[[i]] # valor inicial
      if (length(iv) > 1) paste0(iv, collapse = " - ")
    #   observeEvent(input[[i]], {
        if (is.null(input[[i]])) return()
        input_value <- input[[i]]
        if (length(input_value) > 1) input_value <- paste0(input_value, collapse = " - ")
        df_change <- NULL
        ind_change <- input_value == initial_values[[i]] #indicador de cambio
        if (!ind_change) {
          df_change <- data.frame(id = i, change_by = input_value)
        }
        #print(df_change)
        df_change
    #})

   })
   plyr::compact(l)
}

#' @export
indexButtonsUI <- function(id, list_inputs = NULL, dic_inputs = NULL, class_label="index-label", class_close = "index-close") {

  ns <- shiny::NS(id)


  if (is.null(list_inputs)) return()
  if (identical(list(), list_inputs)) return()
  if (!is.null(dic_inputs)) {
    if (ncol(dic_inputs) > 2) stop("the dictionary should only contain the id of the input and the label")
    names(dic_inputs) <- c("id_input", "label_input")
  }

  purrr::map(seq_along(list_inputs), function(l) {
    id_i <- list_inputs[[l]]$id
    valor_i <- list_inputs[[l]]$change_by
    inputs_id <- ns(paste0("index-", id_i))
    inputs_label <- id_i
    if (!is.null(dic_inputs)) {
      inputs_label <- dic_inputs[grep(id_i, dic_inputs$id_input),2]
    }
    div(class = "index-buttons",
    shiny::actionButton(inputId = inputs_id,
                        label = HTML(paste("<span class=", class_label, ">",inputs_label,"</span>", valor_i, "<span class=", class_close ,">x</span>")),
                        class = "btn action-button")
    )
  })

}

#' @export
indexButtonsServer <- function(session, input, id, parmesan = NULL) {

  buttonsId <- paste0(id, "-index-", parmesan::parmesan_input_ids(parmesan = parmesan))

  purrr::map(buttonsId, function(btn) {
    observeEvent(input[[btn]], {
      id_reset <- gsub(paste0(id, "-index-"), "", btn)
      df_inputs <- parmesan:::initial_inputs_namespace(parmesan_inputs())
      df_inputs <- df_inputs %>% dplyr::filter(id %in% id_reset)
      parmesan:::updateInput_function(session, df_inputs = df_inputs)
    })
  })


}

