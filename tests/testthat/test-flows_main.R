withr::local_package("mockery")
withr::local_package("checkmate")

local_log_dir()
local_flows_data_table()

test_that("flows_get_job gets the corresponding row from the flows table as a list", {
  expect_list(flows_get_job("Dummy workflow", "Job 1"))
  expect_length(flows_get_job("Dummy workflow", "Job 1")$`if`, 0)
  expect_equal(flows_get_job("Dummy workflow", "Job 3")$`if`, TRUE)
})

# flows_update_job --------------------------------------------------------

test_that("flows_update_job updates the corresponding row in the flows table", {
  flows_update_job("Dummy workflow", "Job 1", list(status = "Running", start_time = now()))
  expect_equal(flows_get_job("Dummy workflow", "Job 1")$status, "Running")
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
})

test_that("flows_update_job updates the corresponding row in the flows database", {
  db <- flows_log_get_last_run("Dummy workflow", "Job 1")
  flows_update_job("Dummy workflow", "Job 1", list(status = "Finished", end_time = now()))
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$status, "Finished")
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$start_time, db$start_time)
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
})
