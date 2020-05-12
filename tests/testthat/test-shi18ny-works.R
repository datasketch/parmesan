test_that("shi18ny works", {

  library(shi18ny)
  path <- system.file("examples", "ex03-shi18ny", "parmesan", package = "parmesan")
  parmesan <- parmesan::parmesan_load(path)

  localeDir <- system.file("examples", "ex03-shi18ny", "locale", package = "parmesan")
  opts <- list(
    localeDir = localeDir,
    defaultLang = "es",
    fallbacks = list("es" = "en")
  )
  config <- i18nConfig(opts)
  i18n <- i18nLoad(opts)

  parmesan_es <- i_(parmesan, "es", i18n = i18n)
  parmesan_es

})
