test_that("tessiflow.d directory exists", {
  expect_true(dir.exists(config::get("tessiflow.d")))
})
