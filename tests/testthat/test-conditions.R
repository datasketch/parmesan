test_that("multiplication works", {

  expect_true(eval_conditions(1, "equals", 1))
  expect_false(eval_conditions(1, "not_equals", 1))
  expect_true(eval_conditions( "a", "is_any_of", letters))
  expect_true(eval_conditions( "A", "is_none_of", letters))
  expect_true(eval_conditions( "grRRRR", "contains", "R"))

  expect_true(eval_conditions( "A", "contained_in", "ahhA!"))
  expect_true(eval_conditions( "A", "contained_in_all_of", c("ahA", "A!")))
  expect_true(eval_conditions( "A", "contained_in_any_of", c("ahh", "A!")))
  expect_true(eval_conditions( "B", "contained_in_none_of", c("ahh", "A!")))

  expect_true(eval_conditions( 1, "in", 1:3))
  expect_true(eval_conditions( 4, "not_in", 1:3))

})
