test_that("reactives are replaced",{

  path <- system.file("examples","ex04","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  par_input <- parmesan[["controls_dark"]]$inputs[[1]]
  expect_true(input_has_reactives(par_input))

  datasetNColsLabel <- function()"REACTIVE LABEL"
  datasetNCols <- function() 42
  par_input2 <- replace_reactives(par_input, env = environment())
  expect_equal(par_input2$input_params$label, "REACTIVE LABEL")
  expect_equal(par_input2$input_params$max, 42)

})


