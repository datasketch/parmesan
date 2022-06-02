index_inputs <- function(session, input, parmesan = NULL, disincludeInputs = NULL, env = parent.frame()) {

  parmesan <- parmesan
  initial_values <- parmesan::parmesan_input_values(parmesan = parmesan)%>% plyr::compact()
  #print(initial_values)
  l <- purrr:::map(names(initial_values), function(i) {
    iv <- initial_values[[i]] # valor inicial
    iv <- evaluate_reactive(iv, env = env) %>% plyr::compact()

    #print(iv)
    if (length(iv) > 1) iv <- paste0(iv, collapse = " - ")
    #   observeEvent(input[[i]], {
    if (is.null(input[[i]])) return()
    input_value <- input[[i]]

    if (length(input_value) > 1) input_value <- paste0(input_value, collapse = " - ")
    df_change <- NULL
    ind_change <- input_value == iv#initial_values[[i]] #indicador de cambio
    if (!ind_change) {
      if (!is.null(disincludeInputs)) {
        if (i %in% disincludeInputs) {
          df_change <- NULL
        } else {
          df_change <- data.frame(id = i, change_by = input_value)
        }
      }
    }
    df_change
    #})

  })
  plyr::compact(l)
}

#' @export
indexButtonsUI <- function(id,
                           label = NULL,
                           list_inputs = NULL,
                           dic_yaml = NULL,
                           img_icon = NULL,
                           class_label="index-label",
                           class_close = "index-close", ...) {

  ns <- shiny::NS(id)


  if (is.null(img_icon)) {
    img_icon <- paste0("</span><span class=", class_close ,">x</span>")
  } else {
    img_icon <- shiny::img(src= img_icon, class = class_close)
  }

  if (is.null(list_inputs)) return()
  if (identical(list(), list_inputs)) return()
  tagList(
    div(id = "labelIndex", label),
    div(class = "index-buttons",
        purrr::map(seq_along(list_inputs), function(l) {
          id_i <- list_inputs[[l]]$id
          valor_i <- list_inputs[[l]]$change_by
          inputs_id <- ns(paste0("index-", id_i))
          inputs_label <- id_i
          choices_label <- valor_i
          if (!is.null(dic_yaml)) {
            inputs_label <- dic_yaml[[id_i]]
            if (is.null(inputs_label)) inputs_label <- id_i
            choices_label <- dic_yaml[[as.character(valor_i)]]
            if (is.null(choices_label)) choices_label <- valor_i
          }

          shiny::actionButton(inputId = inputs_id,
                              label = HTML(paste("<div class='content-index'><span class=", class_label, ">",inputs_label,"</span><span class='index-choices'>", choices_label, "</span>", img_icon, "</div>")),
                              class = "index-btn"
          )
        })
    )
  )

}

#' @export
indexButtonsServer <- function(session, input, id, parmesan_ids = NULL, parmesan_load = NULL, module_id = NULL) {
  ns <- session$ns
  buttonsId <- NULL
  if (is.null(parmesan_ids)) {
    buttonsId <- paste0(id, "-index-", parmesan::parmesan_input_ids())
  } else {
    buttonsId <- paste0(id, "-index-", parmesan_ids)
  }

  parmesan:::parmesan_inputs(parmesan = parmesan_load)
  purrr::map(buttonsId, function(btn) {

    observeEvent(input[[btn]], {
      id_reset <- gsub(paste0(id, "-index-"), "", btn)
      df_inputs <- parmesan:::initial_inputs_namespace(parmesan:::parmesan_inputs(parmesan = parmesan_load))
      df_inputs <- df_inputs %>% dplyr::filter(id %in% id_reset)
      parmesan:::updateInput_function(session, df_inputs = df_inputs, parmesan_load, module_id = module_id)
    })
  })


}

