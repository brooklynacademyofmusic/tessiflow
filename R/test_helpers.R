make_fixtures <- function() {
  flow_name <- job_name <- NULL

  files <- dir(testthat::test_path("tessiflow.d"), pattern = "*.yml", full.names = TRUE, recursive = TRUE)
  flows <- lapply(files, yaml::read_yaml)
  saveRDS(flows, testthat::test_path("flows.Rds"))

  flows_data_table <- flows_parse(testthat::test_path("tessiflow.d"))
  saveRDS(flows_data_table, testthat::test_path("flows_data_table.Rds"))

  jobs <- with(flows_data_table, data.table(flow_name, job_name, status = "Waiting", retval = NA_integer_, start_time = now(), end_time = NA_real_))
  saveRDS(jobs, testthat::test_path("jobs.Rds"))
}

local_log_dir <- function(envir = parent.frame()) {
  withr::local_envvar(R_CONFIG_FILE = "config-tessiflow.yml", envir = envir)
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
