test_that("assert_flow_job_name loads flows if they don't exist", {
  expect_true(is.null(tessiflow$flows))
  expect_error(assert_flow_job_name("test","test"))
  expect_true(!is.null(tessiflow$flows))
})

test_that("assert_flow_job_name checks that flow_name and job_name are strings of length 1", {
  expect_error(assert_flow_job_name(c("test","test")),"flow_name.+length 2")
  expect_error(assert_flow_job_name(c(1,2)),"flow_name.+character")
  expect_error(assert_flow_job_name("Dummy workflow",c("test","test")),"job_name.+length 2")
  expect_error(assert_flow_job_name("Dummy workflow",c(1,2)),"job_name.+character")
})

test_that("assert_flow_job_name checks that flow_name is in tessiflow$flows", {
  expect_error(assert_flow_job_name("test"),"flow_name.+Dummy workflow")
})

test_that("assert_flow_job_name checks that job_name is in tessiflow$flows[flow_name]", {
  expect_error(assert_flow_job_name("Dummy workflow","test"),"job_name.+Job 1")
})