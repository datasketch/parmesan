context("Parmesan load")

test_that("Config files are ok", {


  path <- system.file("test_parmesan", "parmesan1", package = "parmesan")
  expect_error(parmesan_load(path),
                 "inputs in layout.yaml not defined in inputs.yaml: gender, age")
  path <- system.file("test_parmesan", "parmesan2", package = "parmesan")
  expect_warning(parmesan_load(path),
                 "inputs in inputs.yaml not defined in layout.yaml: gender")

})







