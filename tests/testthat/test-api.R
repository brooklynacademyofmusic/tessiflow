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
  
  expect_length(res,6)
  expect_equal(rbindlist(res, fill = T)$status,c("Forced start",rep("Waiting",4),"Forced stop"))
})

