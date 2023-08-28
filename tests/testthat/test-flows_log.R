withr::local_package("mockery")
withr::local_package("checkmate")
withr::defer(flows_log_close())

local_log_dir()

test_that("flows_log_create creates a new database table", {
  stub(flows_log_open, "flows_log_create", TRUE)
  flows_log_open()
  flows_log_create()
  expect_true("jobs" %in% DBI::dbListTables(tessiflow$db))
})

test_that("flows_log_create creates an index on the table", {
  expect_equal(DBI::dbGetQuery(tessiflow$db, "pragma index_list(jobs);") %>% nrow(), 1)
})


# flows_log_open ----------------------------------------------------------

test_that("flows_log_open errors when the configuration option isn't set or the directory doesn't exist", {
  expect_error(flows_log_open("dirdoesntexist"), "config.+directory.+log")
  stub(flows_log_open, "config::get", "blah")
  expect_error(flows_log_open(), "config.+directory.+log")
})

test_that("flows_log_open opens a database connection", {
  flows_log_open()
  expect_class(tessiflow$db, "SQLiteConnection")
})

# flows_low_get_last_run --------------------------------------------------

sqlite_upsert("jobs", readRDS(test_path("jobs.Rds")))
test_that("flows_log_get_last_run reports the last run by flow and job", {
  start_time <- as.double(now())

  sqlite_upsert("jobs", data.frame(
    flow_name = "Dummy workflow",
    job_name = "Job 1",
    status = "Waiting",
    start_time = start_time,
    end_time = NA,
    retval = NA
  ))
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 7)
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$start_time, start_time)
})

# flows_log_cleanup -------------------------------------------------------

test_that("flows_log_cleanup corrects the status of jobs in the log that are no longer running.", {
  start_time <- as.double(now())
  test_jobs <- data.frame(
    flow_name = "Dummy workflow",
    job_name = c("Running job", "Killed job"),
    status = "Running",
    start_time = start_time,
    end_time = NA,
    retval = NA,
    pid = c(123, 456)
  )

  sqlite_upsert("jobs", test_jobs)
  query <- tbl(tessiflow$db, "jobs") %>% filter(flow_name == "Dummy workflow")

  stub(flows_log_cleanup, "ps::ps_find_tree", c(123, 456))
  stub(flows_log_cleanup, "ps::ps_pid", c(123, 456))

  flows_log_cleanup()

  expect_names(collect(filter(query, status == "Running"))$job_name, permutation.of = c("Running job", "Killed job"))

  rm(flows_log_cleanup)
  stub(flows_log_cleanup, "ps::ps_find_tree", c(123))
  stub(flows_log_cleanup, "ps::ps_pid", c(123))

  flows_log_cleanup()

  expect_equal(collect(filter(query, status == "Running"))$job_name, "Running job")
  expect_equal(collect(filter(query, status == "Cancelled"))$job_name, "Killed job")
})

test_that("flows_log_cleanup leaves the database in a state so that cancelled jobs will get re-run.", {
  # Simple case, no dependencies
  local_flows_data_table()
  tessiflow$flows[1,job_name := "Killed job"]
  
  job_start <- mock()
  stub(job_maybe_start,"job_start",job_start)
  stub(job_maybe_start,"now",now() + ddays(1))

  job_maybe_start("Dummy workflow","Killed job")
  
  expect_length(mock_args(job_start),1)

  # Now do it again with dependencies
  local_flows_data_table()
  tessiflow$flows[2,job_name := "Killed job"]
  
  flows_update_job("Dummy workflow","Job 1", list(start_time = now(), end_time = now(), retval = 0))
  
  job_maybe_start("Dummy workflow","Killed job")
  
  expect_length(mock_args(job_start),2)
  
})
