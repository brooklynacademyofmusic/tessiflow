test_that("tesitask.d directory exists", {
  expect_true(dir.exists(config::get("tessitask.d")))
})
