test_that("Parmesan shiny", {

  library(shi18ny)

  path <- system.file("examples", "ex03", "parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  i18n <- list(
    defaultLang = "en",
    availableLangs = c("es","en"),
    localeDir = system.file("examples", "ex03", "locale", package = "parmesan")
  )
  i18n <- i18nLoad(i18n)
  i_("hello", lang = "es", i18n = i18n)


  parmesan_es <- i_(parmesan, lang = "es", i18n = i18n, keys = "label")

  expect_equal(parmesan_es$controls$label, "Controles")
  expect_equal(parmesan_es$controls$id, "controls")
  expect_equal(parmesan_es$controls$inputs[[1]]$id, "dataset")
  expect_equal(parmesan_es$controls$inputs[[1]]$input_params$label, "Datos")



})
