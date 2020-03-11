context("Parmesan Configuration")

test_that("Config files are ok", {

  library(shiny)

  config_path <- system.file("examples", "ex01", "parmesan", package = "parmesan")
  cfg <- parmesan_config(config_path = config_path)
  expect_equal(names(cfg), c("inputs", "params"))
  html <- parmesan_render_ui(config_path = config_path)

  expect_equal(parmesan_input_ids(config_path = config_path), c("dataset", "column", "bins"))

  expect_equal(
    htmltools::tagGetAttribute(html[[1]]$children[[1]], "id"),
    "Controls"
  )

  parmesan_input_ids()

  # Parmesan files not found
  # expect_error()


})

test_that("Sections work", {

  config_path <- system.file("examples", "ex02", "parmesan", package = "parmesan")
  parmesan_render_ui(section = "Controls", config_path = config_path)

  config_path <- system.file("examples", "ex02", "parmesan", package = "parmesan")
  parmesan_render_ui(section = "Controls Dark", config_path = config_path)
})

test_that("Sections with dependencies work", {

  config_path <- system.file("examples", "ex04", "parmesan", package = "parmesan")
  parmesan_render_ui(config_path = config_path)

  config_path <- system.file("examples", "ex04", "parmesan", package = "parmesan")
  parmesan_render_ui(section = "Controls Dark", config_path = config_path)
})










