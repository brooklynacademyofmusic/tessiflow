withr::local_package("checkmate")
withr::local_package("mockery")
withr::defer(api_server$kill())  

api_stdout <- tempfile()
api_stderr <- tempfile()

api_server <- callr::r_bg(function() {
  local_log_dir()
  jobs <- readRDS(testthat::test_path("jobs.Rds"))
  jobs[, `:=`(
    start_time = now() - lubridate::days(2),
    end_time = now() - lubridate::ddays(1),
    retval = 0
  )]
  sqlite_upsert("jobs",jobs)
  api_start(docs=TRUE,debug=TRUE)
},package = "tessiflow",
stdout = api_stdout,
stderr = api_stderr)

read_from_log <- function() {
  while(max(file.mtime(api_stdout),file.mtime(api_stderr)) > now() - 1 | file.size(api_stdout)+file.size(api_stderr) == 0) {
    Sys.sleep(1)
  }
  list(out=readLines(api_stdout),err=readLines(api_stderr))
}

test_that("api_server is running", {
  output <- read_from_log()
  expect_match(output$err,"Running plumber API",all=FALSE)
  expect_failure(expect_match(output$err,"Error",all=FALSE))
})

test_that("tessiflow_job_start starts job", {
  res <- tessiflow_job_start("test","test")
  expect_match(res$message,"flow_name.+must be a subset of.+Dummy",all=FALSE)
  
  res <- tessiflow_job_start("Dummy workflow","Job 1")
  expect_equal(res[[1]]$status,"Forced start")
})

test_that("tessiflow_job_stop stops job", {
  res <- tessiflow_job_stop("test","test")
  expect_match(res$message,"flow_name.+must be a subset of.+Dummy",all=FALSE)
  
  res <- tessiflow_job_stop("Dummy workflow 2","Job 3")
  expect_equal(res[[1]]$status,"Forced stop")
})

test_that("tessiflow_flows_get gets the last run of all configured flows", {
  res <- tessiflow_flows_get()
  expect_data_table(res)
  expect_equal(dim(res),c(6,6))
  expect_class(res$start_time,"POSIXct")
  expect_equal(res$status,c("Forced start",rep("Waiting",4),"Forced stop"))
})

test_that("tessiflow_job_start, tessiflow_job_stop do not satisfy the needs of job_maybe_start", {
  # This is a bit of an integration test, checking that force start/stop plays nicely with job_maybe_start
  local_flows_data_table()
  local_log_dir()
  
  job_start <- mock()
  stub(job_maybe_start,"job_start",job_start)
  
  flows_update_job("Dummy workflow", "Job 2", list(needs = "Job 1", scheduled_runs = list(NULL)))
  job_maybe_start("Dummy workflow", "Job 2")
  expect_length(mock_args(job_start),0)
  
  expect_message(
    api_job_start("Dummy workflow", "Job 1"),
    "Loading flows")
  job_maybe_start("Dummy workflow", "Job 2")
  expect_length(mock_args(job_start),0)
  
  api_job_stop("Dummy workflow", "Job 1")
  job_maybe_start("Dummy workflow", "Job 2")
  expect_length(mock_args(job_start),0)
})

test_that("tessiflow_job_start, tessiflow_job_stop, and tessiflow_flows_get work with changed workflow definitions",{
  res <- tessiflow_job_start("Dummy workflow 3","Job 1")
  expect_match(res$message,"flow_name.+must be a subset of.+Dummy",all=FALSE)
  res <- tessiflow_job_stop("Dummy workflow 3","Job 1")
  expect_match(res$message,"flow_name.+must be a subset of.+Dummy",all=FALSE)
  res <- tessiflow_flows_get()
  expect_length(res,6)
  
  withr::local_file(rprojroot::find_testthat_root_file("tessiflow.d/dummy3.yml"))
  system2("sed",c("'s/Dummy workflow 2/Dummy workflow 3/'",rprojroot::find_testthat_root_file("tessiflow.d/dummy2.yml")),
          rprojroot::find_testthat_root_file("tessiflow.d/dummy3.yml"))

  expect_equal(tessiflow_job_start("Dummy workflow 3","Job 1")[[1]]$flow_name,"Dummy workflow 3")
  expect_equal(tessiflow_job_stop("Dummy workflow 3","Job 1")[[1]]$flow_name,"Dummy workflow 3")
  expect_equal(nrow(tessiflow_flows_get()),7) # only those that have been started are listed
  
})

