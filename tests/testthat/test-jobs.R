withr::local_package("mockery")
withr::local_package("lubridate")

local_log_dir()
local_flows_data_table()

flow_name <- tessiflow$flows[1, flow_name]
job_name <- tessiflow$flows[1, job_name]

job_true <- list()
job_true$scheduled_runs <- list(
  cron = c(now()),
  cron = c(now() + ddays(1))
)
job_true$`if` <- "1 == 1"
job_true$`runs-on` <- Sys.info()["nodename"]
job_true$needs <- list("Job 2")

job_false <- list()
job_false$scheduled_runs <- list(
  cron = c(now() - dyears(1)),
  cron = c(now() + ddays(1))
)
job_false$`if` <- "1 == 0"
job_false$`runs-on` <- "anothermachine"
job_false$needs <- list("Job 3", "notajob")

test_that("job_maybe_start runs jobs when runs-on matches the current machine", {
  local_flows_data_table()
  job_start <- mock()
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, `runs-on` := job_false$`runs-on`]
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `runs-on` := job_true$`runs-on`]
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

test_that("job_maybe_start runs jobs when if is true", {
  local_flows_data_table()
  job_start <- mock()
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, `if` := job_false$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `if` := job_true$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

jobs <- readRDS(test_path("jobs.Rds"))
jobs[, `:=`(
  end_time = now() - lubridate::ddays(1),
  retval = 0
)]
last_run_times <- function(flow, job) {
  jobs[flow_name == "Dummy workflow" & job_name == job, ]
}
stub(job_maybe_start, "flows_log_get_last_run", last_run_times)

test_that("job_maybe_start runs jobs when needs are met and retval is 0", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, needs := list(job_false$needs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, needs := list(job_true$needs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  jobs[job_name == "Job 2", `:=`(end_time = now())]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

test_that("job_maybe_start runs jobs when needs are met and retval <> 0, but only if `if` is true", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, needs := list(job_true$needs)]
  jobs[job_name == "Job 2", `:=`(end_time = now(), retval = 1)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `if` := job_true$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

test_that("job_maybe_start runs jobs when they are scheduled", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, scheduled_runs := list(job_false$scheduled_runs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, scheduled_runs := list(job_true$scheduled_runs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

if(FALSE) {
# flows_get_job -----------------------------------------------------------

test_that("flows_get_job returns a list of job parameters")


# flows_update_job --------------------------------------------------------

test_that("flows_update_job updates the flows data.table")

test_that("flows_update_job writes to the log")

# job_start ---------------------------------------------------------------

test_that("job_start spins up an r session")

test_that("job_start spins up an r session but only if one doesn't already exist")

test_that("job_start updates the flows data.table and database")

test_that("job_start writes to the log file")

test_that("job_start calls job_step")

# job_step ----------------------------------------------------------------

test_that("job_start calls the next step")

test_that("job_start updates the flows data.table and database")

test_that("job_start writes to the log file")

test_that("job_start does nothing if `if` evaluates to FALSE")

test_that("job_start passed on the flow and step right environment variables")

# job_poll ----------------------------------------------------------------

test_that("job_poll reads from stdout and writes to the log")

test_that("job_poll reads from stderr and writes to the log")

test_that("job_poll updates the flows data.table and database on error")

test_that("job_poll calls job_step if it's ready to advance")

test_that("job_poll calls job_finalize if it's done or errored")
}