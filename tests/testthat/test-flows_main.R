withr::local_package("mockery")

local_log_dir()

flows <- readRDS(test_path("flows_data_table.Rds")) 
job <- flows[1,]
job$scheduled_runs = list(list())  
job$`if` = list(list(list()))
job$jobs = list(list(list()))
job$needs = list(list(list()))

test_that("job_maybe_run runs jobs when runs-on matches the current machine", {
  job_run <- mock()
  stub(job_maybe_run,"job_run",job_run)
  
  job$jobs = list(list(list("runs-on" = "notthismachine")))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),0)
  
  job$jobs = list(list(list("runs-on" = Sys.info()["nodename"])))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),1)
})

test_that("job_maybe_run runs jobs when if is true", {
  job_run <- mock()
  stub(job_maybe_run,"job_run",job_run)
  
  job$jobs = list(list(list("if" = "1 == 0")))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),0)
  
  job$jobs = list(list(list("if" = TRUE)))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),1)
})

test_that("job_maybe_run runs jobs when needs are met and retval is 0", {
  job_run <- mock(cycle = TRUE)
  stub(job_maybe_run,"job_run",job_run)
  
  jobs <- readRDS(test_path("jobs.Rds"))
  jobs[,`:=`(end_time=now() - lubridate::ddays(1),
             retval = 0)]
  last_run_times <- function(flow,job) { jobs[flow_name == "Dummy workflow" & job_name == job,] }
  stub(job_maybe_run,"flows_log_get_last_run",last_run_times)
  
  job$jobs = list(list(list("needs" = "anotherjob")))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),0)
  
  job$jobs = list(list(list("needs" = "Job 2")))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),0)
  
  jobs[job_name == "Job 2", `:=`(end_time=now())]
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),1)
})

test_that("job_maybe_run runs jobs when needs are met and retval <> 0, but only if `if` is true", {
  job_run <- mock(cycle = TRUE)
  stub(job_maybe_run,"job_run",job_run)
  
  jobs <- readRDS(test_path("jobs.Rds"))
  jobs[,`:=`(end_time=now() - lubridate::ddays(1),
             retval = 0)]
  last_run_times <- function(flow,job) { jobs[flow_name == "Dummy workflow" & job_name == job,] }
  stub(job_maybe_run,"flows_log_get_last_run",last_run_times)
  
  job$jobs = list(list(list("needs" = "Job 2")))
  jobs[job_name == "Job 2", `:=`(end_time = now(), retval = 1)]
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),0)
  
  job$jobs[[1]]$`if` = "TRUE"
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),1)
})

test_that("job_maybe_run runs jobs when they are scheduled", {
  job_run <- mock(cycle = TRUE)
  stub(job_maybe_run,"job_run",job_run)
  
  jobs <- readRDS(test_path("jobs.Rds"))
  jobs[,`:=`(end_time=now() - lubridate::ddays(1),
             retval = 0)]
  last_run_times <- function(flow,job) { jobs[flow_name == "Dummy workflow" & job_name == job,] }
  stub(job_maybe_run,"flows_log_get_last_run",last_run_times)
  
  job$scheduled_runs = list(list(now() - lubridate::ddays(2)))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),0)
  
  job$scheduled_runs = list(list(now() + lubridate::ddays(1)))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),0)
  
  job$scheduled_runs = list(list(now() + lubridate::ddays(0)))
  
  job_maybe_run(job)
  expect_length(mock_args(job_run),1)
})

