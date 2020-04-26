context("Parmesan load")

test_that("Config files are ok", {


  path <- system.file("test_parmesan", "parmesan1", package = "parmesan")
  expect_error(parmesan_load(path),
                 "inputs in layout.yaml not defined in inputs.yaml: gender, age")


})







