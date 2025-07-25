#' database
#' @title Tessiflow database utilities
#' @name database
#'
#' @param flows_log_dir directory where the SQLite database is stored
#' @importFrom dbplyr db_collect
{}
#' @return invisible
#' @describeIn database opens the jobs database connection
flows_log_open <- function(flows_log_dir = config::get("tessiflow.log")) {
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir)) {
    stop("Please set the tessiflow.log configuration option to a directory where log files will be stored")
  }

  if (is.null(tessiflow$db)) {
    tessiflow$db <- DBI::dbConnect(RSQLite::SQLite(), file.path(flows_log_dir, "tessiflow.sqlite"))
    # Set sqlite timeout to 5 seconds
    RSQLite::sqliteSetBusyHandler(tessiflow$db,5000) 
  }

  if (!DBI::dbExistsTable(tessiflow$db, "jobs")) {
    flows_log_create()
  }

  invisible()
}


#' @describeIn database closes the jobs database connection
flows_log_close <- function() {
  if (!is.null(tessiflow$db)) {
    DBI::dbDisconnect(tessiflow$db)
  }
  tessiflow$db <- NULL
}

#' @describeIn database Create the SQLite log table
flows_log_create <- function() {
  DBI::dbCreateTable(tessiflow$db, "jobs", fields = c(
    flow_name = "character",
    job_name = "character",
    `runs-on` = "character",
    steps = "character",
    needs = "character",
    `if` = "character",
    status = "character",
    start_time = "double",
    end_time = "double",
    retval = "integer",
    step = "integer",
    pid = "integer",
    tempdir = "character",
    `timeout-minutes` = "integer"
  ))

  DBI::dbExecute(tessiflow$db, "CREATE UNIQUE INDEX jobs_index ON jobs(flow_name,job_name,start_time)")
}

#' flows_log_get_last_run
#'
#' @param flows character vector of flows
#' @param jobs character vector of jobs
#'
#' @return matching rows from the log database for the last run of each flow, job pair
#' @importFrom checkmate assert_character
#' @importFrom dplyr tbl inner_join group_by slice_max collect
#'
flows_log_get_last_run <- function(flows, jobs) {
  flow_name <- job_name <- start_time <- NULL

  assert_character(flows, any.missing = FALSE)
  assert_character(jobs, any.missing = FALSE)
  flows_log_open()

  query_tbl <- data.frame(flow_name = flows, job_name = jobs)

  tbl(tessiflow$db, "jobs") %>%
    group_by(flow_name, job_name) %>%
    slice_max(start_time, n = 1) %>%
    collect() %>% inner_join(query_tbl, by = c("flow_name", "job_name"))
}

#' flows_log_get_create_time
#'
#' @return creation time of the log database
#'
flows_log_get_create_time <- function() {
  flows_log_open()

  file.info(DBI::dbGetInfo(tessiflow$db)$dbname)$ctime
}

#' flows_log_cleanup
#'
#' Cleans up jobs in the log that are marked as `Running` but no longer are.
#'
#' @return invisibly
#' @importFrom lubridate now
flows_log_cleanup <- function() {
  status <- pid <- flow_name <- job_name <- start_time <- NULL

  flows_log_open()

  tree <- ps::ps_find_tree("tessiflow-daemon")

  pids <- sapply(tree, ps::ps_pid)

  data <- tbl(tessiflow$db, "jobs") %>%
    filter(status == "Running" & !pid %in% pids) %>%
    mutate(status = "Cancelled") %>%
    select(flow_name, job_name, start_time, status) %>%
    collect() %>% setDT()
  
  data[,`:=`(retval = -1,
             end_time = now())]

  if (nrow(data) > 0) {
    sqlite_upsert("jobs", data, tessiflow$db)
  }

  invisible()
}
