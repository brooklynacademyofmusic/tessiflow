withr::local_package("mockery")
withr::local_package("checkmate")

local_log_dir()
local_flows_data_table()


# flows_main --------------------------------------------------------------



# flows_get_job -----------------------------------------------------------

test_that("flows_get_job returns a list of job parameters", {
  job <- flows_get_job("Dummy workflow", "Job 1")
  expect_names(names(job), permutation.of = c(
    "flow_name", "job_name", "steps",
    "runs-on", "needs", "if", "env",
    "on.schedule", "scheduled_runs",
    "retval", "start_time", "end_time", "status"
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
  expect_error(flows_get_job("not a workflow", "not a job"), "not a workflow")
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
