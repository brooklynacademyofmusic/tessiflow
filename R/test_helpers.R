make_fixtures <- function() {
  
  files <- dir(testthat::test_path("tessiflow.d"), pattern="*.yml", full.names = TRUE, recursive = TRUE)
  flows <- lapply(files, yaml::read_yaml)
  saveRDS(flows,testthat::test_path("flows.Rds"))
  
  flows_data_table <- flows_parse(testthat::test_path("tessiflow.d"))
  saveRDS(flows_data_table,testthat::test_path("flows_data_table.Rds"))
  
  jobs <- with(flows_data_table,data.table(flow_name,job_name,status="Waiting",retval=NA_integer_,start_time=now(),end_time=NA_real_))
  saveRDS(jobs,testthat::test_path("jobs.Rds"))
}

local_log_dir <- function(envir=parent.frame()) {
  dir.create(file.path(tempdir(),"logs"))
  withr::defer({
    flows_log_close()
    gc()
    unlink(file.path(tempdir(),"logs"),recursive = TRUE, force = TRUE)
  },envir=envir)
}