withr::local_package("mockery")
withr::local_package("checkmate")

local_log_dir()


# flows_main --------------------------------------------------------------

# Get rid of error control since we use errors to break out of the main loop
stub(flows_main, "try_fetch", function(expr,...){eval(expr)})

test_that("flows_main actually *does* something", {
  stub(flows_main, "flows_auto_refresh", function(){tessiflow$flows <- flows_parse()})
  m <- mock()
  stub(flows_main, "job_maybe_start", m)
  stub(flows_main, "job_poll", m)
  stub(flows_main, "Sys.sleep", function(...) {
    stop("first loop")
  })
  
  expect_error(flows_main(), "first loop")
  expect_equal(length(mock_args(m)), nrow(tessiflow$flows))
})

local_flows_data_table()
flows <- tessiflow$flows

test_that("flows_main does nothing when all tasks are finished", {
  flows[, status := "Finished"]
  stub(flows_main, "flows_auto_refresh", flows)
  m <- mock()
  stub(flows_main, "job_maybe_start", m)
  stub(flows_main, "job_poll", m)
  flows_main()
  expect_equal(length(mock_args(m)), 0)
})

test_that("flows_main calls job_maybe_start when tasks are waiting", {
  flows[, status := "Waiting"]
  stub(flows_main, "flows_auto_refresh", flows)
  m <- mock()
  stub(flows_main, "job_maybe_start", m)
  stub(flows_main, "Sys.sleep", function(...) {
    stop("first loop")
  })
  expect_error(flows_main(), "first loop")
  expect_equal(length(mock_args(m)), nrow(flows))
})

test_that("flows_main calls job_poll when tasks are running", {
  flows[, status := "Running"]
  stub(flows_main, "flows_auto_refresh", flows)
  m <- mock()
  stub(flows_main, "job_poll", m)
  stub(flows_main, "Sys.sleep", function(...) {
    stop("first loop")
  })
  expect_error(flows_main(), "first loop")
  expect_equal(length(mock_args(m)), nrow(flows))
})

test_that("flows_main call job_reset when they are done", {
  flows[, status := rep(c("Finished", "Waiting", "Bloopy"), 2)]
  stub(flows_main, "flows_auto_refresh", flows)
  m <- mock()
  stub(flows_main, "job_reset", m)
  stub(flows_main, "Sys.sleep", function(...) {
    stop("first loop")
  })
  expect_error(flows_main(), "first loop")
  expect_equal(length(mock_args(m)), 4)
})

# flows_get_job -----------------------------------------------------------

test_that("flows_get_job returns a list of job parameters", {
  job <- flows_get_job("Dummy workflow", "Job 1")
  expect_names(names(job), permutation.of = c(
    "flow_name", "job_name", "steps",
    "runs-on", "needs", "if", "env",
    "on.schedule", "scheduled_runs",
    "retval", "start_time", "end_time", 
    "status", "timeout-minutes"
  ))
})

test_that("flows_get_job gets the corresponding row from the flows table as a list", {
  flows <- readRDS(test_path("flows.Rds"))
  flow <- flows[[1]]
  flow$jobs$job1$`if` <- "TRUE"
  flow$jobs$job1$needs <- list("Job 2", "Job 3")
  stub(flows_parse, "yaml::read_yaml", mock(flow, NULL))

  tessiflow$flows <- flows_parse()

  job <- flows_get_job("Dummy workflow", "Job 1")

  expect_character(job$flow_name, len = 1)
  expect_character(job$job_name, len = 1)
  expect_character(job$`runs-on`, len = 1)
  expect_character(job$`if`, len = 1)

  expect_list(job$scheduled_runs, types = "POSIXct")
  expect_list(job$steps, types = c("character", "list"))
  expect_list(job$needs, types = "character")
  expect_list(job$env, types = "character", names = "named")
})

test_that("flows_get_job errors if it can't find the job", {
  expect_error(flows_get_job("not a workflow", "not a job"), "flow_name.+Dummy workflow")
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
