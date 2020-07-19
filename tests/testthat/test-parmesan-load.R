context("Parmesan load")

test_that("Config files are ok", {


  path <- system.file("test_parmesan", "parmesan1", package = "parmesan")
  expect_error(parmesan_load(path),
                 "inputs in layout.yaml not defined in inputs.yaml: gender, age")
  path <- system.file("test_parmesan", "parmesan2", package = "parmesan")
  expect_warning(parmesan_load(path),
                 "inputs in inputs.yaml not defined in layout.yaml: gender")

  parmesan <- parmesan_load(path)
  parmesan_input_ids(parmesan = parmesan)

  presets <- list(
    bins = list(min = 1),
    column = list(label ="COL")
  )
  parmesan <- parmesan_load(path, presets = presets)

  expect_equal(parmesan$controls$inputs[[3]]$input_params[["min"]],
               presets[["bins"]]$min)
  expect_equal(parmesan$controls$inputs[[2]]$input_params[["label"]],
               presets[["column"]]$label)


})







