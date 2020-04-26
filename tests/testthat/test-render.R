test_that("render works", {

  library(tidyverse)
  library(shiny)

  path <- system.file("examples","ex02","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  inputs <- yaml::read_yaml(file.path(path, "inputs.yaml"))
  layout <- yaml::read_yaml(file.path(path, "layout.yaml"))

  expect_equal(names(parmesan), names(layout))
  expect_equal(unname(map_chr(parmesan, "id")), names(layout))
  expect_equal(parmesan[[2]]$inputs[[1]], c(list(id = "column"), inputs$column))

  expect_equal(parmesan_input_ids(parmesan = parmesan), c("dataset", "column", "bins"))

  par_input <- parmesan[[1]]$inputs[[1]]

  # Input with no dependencies
  expect_equal(render_par_input(par_input),
               selectInput("dataset", "Dataset",
                           choices = c("rock","pressure","cars"),
                           selected = "rock"))

  section <- "controls_dark"
  rendered_section <- render_section(section, parmesan = parmesan)
  shiny_inputs <- div(
    div(id = "controls_dark", class = "par_section", "Controls Dark"),
    tagList(
      numericInput("column", "Column", value = 1),
      sliderInput("bins", "Bins", min = 0, max = 50, value = 10)
    )
  )
  expect_equal(as.character(rendered_section), as.character(shiny_inputs))


  parmesan_sections <- function(inputs_layout){
    names(inputs_layout)
  }

  render_section


})


test_that("show_if works", {

  path <- system.file("examples","ex04","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)
  render_section("controls", parmesan = parmesan)

  par_input <- parmesan[["controls_dark"]]$inputs[[2]]
  input <- list(plot_type = "Histogram")
  expect_true(validate_show_if(par_input, input))
  input <- list(plot_type = "XXXX")
  expect_false(validate_show_if(par_input, input))

  # Include Bins only when plot_type equals Histogram
  input <- list(plot_type = "Histogram")
  expect_true(grepl("Bins", render_par_input(par_input, input)))
  input <- list(plot_type = "XXXX")
  expect_null(render_par_input(par_input, input))

})


