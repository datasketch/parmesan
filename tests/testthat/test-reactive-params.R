test_that("reactives are replaced",{

  path <- system.file("examples","ex04-reactives","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  par_input <- parmesan[["controls_dark"]]$inputs[[1]]
  expect_true(input_has_reactive_param_values(par_input))

  newenv <- new.env()
  datasetNColsLabel <- local(function() "REACTIVE LABEL", envir = newenv)
  #newenv$datasetNColsLabel <- function() "REACTIVE LABEL"
  datasetNCols <- function() 42
  par_input2 <- replace_reactive_param_values(par_input, env = newenv)
  expect_equal(par_input2$input_params$label, "REACTIVE LABEL")
  expect_equal(par_input2$input_params$max, 42)

})


