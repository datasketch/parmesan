test_that("render works", {

  library(tidyverse)
  library(shiny)

  path <- system.file("examples","ex02-custom-container/","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  inputs <- yaml::read_yaml(file.path(path, "inputs.yaml"))
  layout <- yaml::read_yaml(file.path(path, "layout.yaml"))

  expect_equal(names(parmesan), names(layout))
  expect_equal(unname(map_chr(parmesan, "id")), names(layout))
  expect_equivalent(parmesan[[2]]$inputs[[1]], c(list(id = "column"), inputs$column))

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
    div(id = "controls_dark", class = "par_section",
        div(class = "style_section", style="font-size: medium;font-weight: bolder;",
            "Controls Dark")
        ),
    tagList(
      numericInput("column", "Column", value = 1),
      sliderInput("bins", "Bins", min = 0, max = 50, value = 10)
    )
  )
  expect_equal(as.character(rendered_section$children[1:2]),
               as.character(shiny_inputs$children))


  parmesan_sections <- function(inputs_layout){
    names(inputs_layout)
  }

  render_section


})


test_that("show_if works", {

  path <- system.file("examples","ex04-reactives/","parmesan", package = "parmesan")
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


  # Do not show layouts with empty inputs
  path <- system.file("examples","ex05-reactive-output/","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)
  expect_null(render_section("controls_empty", parmesan = parmesan))

})


test_that("info tooltip works", {

  library(tidyverse)
  library(shiny)

  path <- system.file("examples","ex02-custom-container/","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  inputs <- yaml::read_yaml(file.path(path, "inputs.yaml"))
  layout <- yaml::read_yaml(file.path(path, "layout.yaml"))

  # adding input_info to bins
  parmesan$controls_dark$inputs[[2]]$input_info <- list(icon = "check", text = "Input clarifications")


  # Input with no tooltip
  expect_equal(render_par_input(parmesan$controls$inputs[[1]]),
               selectInput("dataset", "Dataset",
                           choices = c("rock","pressure","cars"),
                           selected = "rock"))


  # Input with tooltip
  expect_equal(render_par_input(parmesan$controls_dark$inputs[[2]]),
               sliderInput("bins", parmesan::infoTooltip(parmesan$controls_dark$inputs[[2]]), 0, 50, 10, 1))

})


test_that("namespace lookup works", {

  expect_equal(input_namespace("colorPaletteInput"), "shinyinvoer::colorPaletteInput")

  expect_equal(input_namespace("actionButton"), "shiny::actionButton")

})


