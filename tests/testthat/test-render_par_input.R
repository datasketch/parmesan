test_that("Helpers",{

  par_input <- list(input_type = "nonExistingInput")
  expect_error(validate_input_type(par_input))

  path <- system.file("examples","ex06-conditions","parmesan", package = "parmesan")
  parmesan <- parmesan_load(path)

  # No dependencies
  par_input <- parmesan[["controls"]]$inputs[[1]] #dataset
  expect_equal(which_parmesan_reactives(par_input),character(0))
  expect_equal(replace_reactive_param_values(par_input), par_input)

  # Show if dependency
  par_input <- parmesan[["controls"]]$inputs[[2]] # num_or_fct
  expect_equal(which_parmesan_reactives(par_input),"hasFctAndNum")
  expect_equal(replace_reactive_param_values(par_input), par_input)

  # par_inputs
  input <- list()
  env <- new.env()
  env$hasFctAndNum <-  local(function() TRUE, env)
  #hasFctAndNum <-  local(function() TRUE, env)

  par_input <- list(show_if = list(`hasFctAndNum()` =list(equals = TRUE)))
  expect_true(validate_show_if(par_input, input, env))

  # par_input <- parmesan[["controls"]]$inputs[[6]] #dataset
  par_input <- list(show_if_any =
                      list(`hasFctAndNum()` =
                             list(equals = TRUE),
                           y = list(greater_than = 2)))
  expect_true(validate_show_if(par_input, input, env))
  par_input <- list(show_if_all =
                      list(`hasFctAndNum()` =
                             list(equals = TRUE),
                           y = list(greater_than = 2)))
  input[["y"]] <- 1
  expect_false(validate_show_if(par_input, input, env))

})


