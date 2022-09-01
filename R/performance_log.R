#' @describeIn flows_log_open opens the performance database connection
performance_log_open <- function(flows_log_dir = config::get("tessiflow.log")) {
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir)) {
    stop("Please set the tessiflow.log configuration option to a directory where log files will be stored")
  }

  if (is.null(tessiflow$db2)) {
    tessiflow$db2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(flows_log_dir, "tessiflow.sqlite"))
  }

  if (!DBI::dbExistsTable(tessiflow$db2, "performance")) {
    performance_log_create()
  }

  invisible()
}

#' @describeIn flows_log_open closes the performance database connection
performance_log_close <- function() {
  if (!is.null(tessiflow$db2)) {
    DBI::dbDisconnect(tessiflow$db2)
  }
  tessiflow$db2 <- NULL
}

#' performance_poll
#'
#' return a list of process data from `python psutil`
#'
#' @param pid integer process id
#'
#' @importFrom stats setNames
#'
#' @return integer list of process data
performance_poll <- function(pid) {
  psutil <- reticulate::import("psutil")
  process <- psutil$Process(as.integer(pid))
  performance <- list(
    cpu_times = process$cpu_times(),
    cpu_percent = process$cpu_percent(),
    # memory_full_info = process$memory_full_info(),
    io_counters = process$io_counters(),
    pid = pid,
    ppid = process$ppid()
  )

  performance <- c(performance,
    memory_full_info = ps::ps_memory_info(ps::ps_handle(pid))
  )

  performance <- lapply(performance, function(o) {
    if (inherits(o, "python.builtin.object")) {
      names <- names(o)
      lapply(names, function(name) {
        o[name]
      }) %>% setNames(names)
    } else {
      o
    }
  }) %>%
    unlist() %>%
    purrr::discard(~ inherits(., "python.builtin.object"))

  return(performance)
}

#' performance_log_update
#'
#' Add rows to the performance log table in the SQLite database by calling performance_poll
#' on all processes in the tessiflow-daemon tree
#'
#' @param pids integer vector of process ids
#'
#' @importFrom dplyr select filter group_by tbl
#' @return number of rows updated
performance_log_update <- function(pids = sapply(
                                     ps::ps_find_tree("tessiflow-daemon"),
                                     ps::ps_pid
                                   )) {
  . <- pid <- status <- start_time <- flow_name <- job_name <- step <- NULL

  performance_data <- rbindlist(lapply(pids, performance_poll)) %>%
    .[, `:=`(timestamp = now())]

  flows_data <- tbl(tessiflow$db2, "jobs") %>%
    filter(pid %in% pids & status == "Running") %>%
    group_by(pid) %>%
    slice_max(start_time, 1) %>%
    select(flow_name, job_name, step, pid)

  sqlite_upsert("performance",
    merge(performance_data, flows_data, by = "pid", all.x = T),
    con = tessiflow$db2
  )
}

#' performance_main
#'
#' Main performance logger loop. Calls `performance_log_update` every 30 seconds
#'
#' @return never
performance_main <- function() {
  performance_log_open()

  while (TRUE) {
    try(performance_log_update())
    Sys.sleep(10)
  }
}

#' @describeIn flows_log_open Create the SQLite performance log table
performance_log_create <- function() {
  flows_log_open()

  if (DBI::dbExistsTable(tessiflow$db2, "performance")) {
    return(invisible())
  }

  performance_poll_fields <- performance_poll(Sys.getpid())
  performance_poll_fields[] <- "double"

  DBI::dbCreateTable(tessiflow$db2, "performance", fields = c(
    flow_name = "character",
    job_name = "character",
    timestamp = "double",
    step = "integer",
    performance_poll_fields
  ))

  DBI::dbExecute(tessiflow$db2, "CREATE UNIQUE INDEX performance_index ON performance(pid,timestamp)")

  invisible()
}
