withr::local_package("mockery")
withr::local_package("checkmate")
withr::defer(flows_log_close())

local_log_dir()

# sqlite_upsert --------------------------------------------------------

test_that("sqlite_upsert complains if the table isn't in the database or doesn't have the join columns", {
  expect_error(sqlite_upsert("notatable", c(x = 1)), "notatable")
  expect_error(sqlite_upsert("jobs", c(notafield = 1)), "include the elements", )
  expect_error(sqlite_upsert("jobs", data.frame(start_time = lubridate::now())), "flow_name.+job_name")
})

test_that("sqlite_upsert writes a new row to the table", {
  sqlite_upsert("jobs", data.frame(
    flow_name = "Dummy workflow",
    job_name = "Job 1",
    status = "Waiting",
    start_time = now(),
    end_time = NA,
    retval = NA
  ))
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
})

test_that("sqlite_upsert writes a row to the table", {
  job <- flows_log_get_last_run("Dummy workflow", "Job 1")
  job$status <- "Running"
  sqlite_upsert("jobs", job)
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$status, "Running")
})
