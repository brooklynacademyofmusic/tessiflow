test_that("tessitask.d directory exists", {
  expect_true(dir.exists(config::get("tessitask.d")))
})
