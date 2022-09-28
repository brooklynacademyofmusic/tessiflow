make_fixtures <- function() {
  flow_name <- job_name <- NULL

  files <- dir(rprojroot::find_testthat_root_file("tessiflow.d"), pattern = "*.yml", full.names = TRUE, recursive = TRUE)
  flows <- lapply(files, yaml::read_yaml)
  saveRDS(flows, rprojroot::find_testthat_root_file("flows.Rds"))

  flows_data_table <- flows_parse(rprojroot::find_testthat_root_file("tessiflow.d"))
  saveRDS(flows_data_table, rprojroot::find_testthat_root_file("flows_data_table.Rds"))

  jobs <- with(flows_data_table, data.table(flow_name, job_name, status = "Waiting", retval = NA_integer_, start_time = now(), end_time = NA_real_))
  saveRDS(jobs, rprojroot::find_testthat_root_file("jobs.Rds"))

  performance <- performance_poll(Sys.getpid())
  saveRDS(performance, rprojroot::find_testthat_root_file("performance.Rds"))
}

local_log_dir <- function(envir = parent.frame()) {
  dirname <- config::get("tessiflow.log")
  dir.create(dirname)
  withr::defer(
    {
      flows_log_close()
      gc()
      unlink(dirname, recursive = TRUE, force = TRUE)
    },
    envir = envir
  )
}

local_flows_data_table <- function(envir = parent.frame()) {
  test_path <- scheduled_runs <- `runs-on` <- needs <- NULL

  tessiflow$flows <- readRDS(test_path("flows_data_table.Rds"))
  tessiflow$flows[1, scheduled_runs := list(list(NULL))]
  tessiflow$flows[1, `if` := list(list(NULL))]
  tessiflow$flows[1, `runs-on` := list(list(NULL))]
  tessiflow$flows[1, needs := list(list(NULL))]

  withr::defer(
    {
      tessiflow$flows <- NULL
    },
    envir = envir
  )
}

