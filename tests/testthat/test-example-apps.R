test_that("01 Basic example", {

  library(shiny)

  path <- system.file("examples", "ex01-basic", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  # Reactives
  datasetInput <- function() "rocks"

  input_ids <- parmesan_input_ids(parmesan)
  inputs <- parmesan_load(path, inputs_only = TRUE)
  html <- render_section(parmesan = parmesan)

  ids_in_html <- lapply(input_ids, function(i) grepl(paste0('id="',i,'"'),html))
  expect_true(all(unlist(ids_in_html)))

})

test_that("02 Custom container", {

  path <- system.file("examples", "ex02-custom-container", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  css <- "background-color:#DDD;border: 2px solid #CCC;border-radius:10px;padding:10px;"
  div_dark <- function(...){
    div(style = css, ...)
  }

  html <- render_section(section = "controls_dark", parmesan = parmesan,
                 container_section = div_dark)
  expect_equal(html$attribs$style, css)

})

test_that("03 Parmesan shiny", {

  library(shi18ny)
  path <- system.file("examples", "ex03-shi18ny", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  i18n <- list(
    defaultLang = "en",
    availableLangs = c("es","en"),
    localeDir = system.file("examples", "ex03-shi18ny", "locale", package = "parmesan")
  )
  i18n <- i18nLoad(i18n)
  i_("hello", lang = "es", i18n = i18n)
  i_(list(label = "hello"), lang = "es", i18n)
  parmesan_es <- i_(parmesan, lang = "es", i18n = i18n, keys = "label")

  expect_equal(parmesan_es$controls$label, "Controles")
  expect_equal(parmesan_es$controls$id, "controls")
  expect_equal(parmesan_es$controls$inputs[[1]]$id, "dataset")
  expect_equal(parmesan_es$controls$inputs[[1]]$input_params$label, "Datos")

})


test_that("04 Reactive inputs",{

  path <- system.file("examples", "ex04-reactives", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)
  parmesan_env <- new.env()

  datasetNColsLabel <- function() "LABEL"
  datasetNCols <- function() "LABEL"

  input <- list(ploy_type = "Plot")
  html <- render_section(section = "controls_dark", parmesan = parmesan,
                 input = input)
  html
  expect_null(html$children[[2]][[2]])

  input <- list(plot_type = "Histogram")
  html <- render_section(section = "controls_dark", parmesan = parmesan,
                         input = input, env = parmesan_env)
  bins_slider <- html$children[[2]][[2]]
  expect_true(grepl('id="bins"', bins_slider))

})

test_that("05 reactive outputs", {

  path <- system.file("examples", "ex05-reactive-output", "parmesan",
                      package = "parmesan")
  parmesan <- parmesan_load(path)
  parmesan_env <- new.env()

  # TODO how to test reactivity?
  # input <- list(ploy_type = "Plot")
  # output <- list()
  # html <- output_parmesan("#all_controls_here", parmesan = parmesan,
  #                 input = input, output = output,
  #                 env = parmesan_env)
  # expect_true(grepl('id="bins"', html))
  # input <- list(plot_type = "Histogram")
  # html <- output_parmesan("#all_controls_here", parmesan = parmesan,
  #                 input = input, output = output,
  #                 env = parmesan_env)
  # expect_true(grepl('id="bins"', html))

})


test_that("06 Reactive inputs and conditions",{

  path <- system.file("examples", "ex06-conditions", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  which_parmesan_reactives(parmesan)

  hasFctAndNum <-  function() TRUE
  datasetColLabels <-  function() "LABELS"
  datasetNumCols <- function() "N"
  datasetFctCols <- function() "N"

  parmesan_alert(parmesan, env = environment(), panic = FALSE)
  expect_error(parmesan_alert(parmesan, panic = TRUE))

  # input <- list()
  # render_section(section = NULL, parmesan = parmesan, input = input)


})




