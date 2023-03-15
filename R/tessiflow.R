tessiflow <- new.env()

#' tessiflow_run
#'
#' Wraps `flows_main()` with logging to `tessiflow-daemon` and environment variable `tessiflow-daemon` and a separate
#' `performance_main()` process.
#' @importFrom withr local_envvar local_dir
#' @return nothing, invisibly
tessiflow_run <- function() {
  flows_log_dir <- config::get("tessiflow.log")
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir)) {
    stop("Please set the tessiflow.log configuration option to a directory where log files will be stored")
  }

  # called for error-checking side-effects
  flows_parse()

  # lock by pid
  tessiflow_pid_lock(flows_log_dir)
  withr::defer(tessiflow_pid_unlock(flows_log_dir))

  logfile <- file.path(flows_log_dir, "tessiflow-daemon.log")
  log_rotate(logfile)

  flows_log_cleanup()

  log_callback <- function(line) {
    # Only log if it's not already being logged
    if (!grepl("^\\[ [\\d\\-]{10} ", line, perl = TRUE)) {
      job_log_write("tessiflow-daemon", lines = line, console = TRUE)
    } else {
      cat(line, sep = "\n")
    }
  }

  local_envvar("tessiflow-daemon" = "YES")
  callr::r(function() {
    performance_logger <- callr::r_bg(performance_main, package = TRUE, poll_connection = TRUE)
    #api <- callr::r_bg(function() {pr("api.R") %>% pr_run(config::get("tessiflow.port"))}, poll_connection = TRUE, stdout = "", stderr = "")
    cat("Starting tessiflow scheduler ...\n")
    flows_main()
  }, callback = log_callback, package = TRUE, stderr = "2>&1")

  invisible()
}

#' @describeIn tessiflow_run Call tessiflow_run from the `R_HOME` directory, and pause on error
#' @export
tessiflow_start <- function() {
  tryCatch(
    {
      local_dir(Sys.getenv("R_USER"))
      tessiflow_run()
    },
    error = function(e) {
      e <- rlang::cnd_entrace(e)
      error_print(e)
      countdown(30)
    }
  )
}

countdown <- function(sec) {
  while (sec > 0) {
    cat(paste0("Pausing for ", sec, " seconds...\r"))
    sec <- sec - 1
    Sys.sleep(1)
  }
}

#' tessiflow_stop
#'
#' @importFrom ps ps_kill_tree
#' @export
tessiflow_stop <- function() {
  ps_kill_tree("tessiflow-daemon")
}

#' tessiflow_enable
#'
#' Schedule a keep-alive job that runs tessiflow_run.
#'
#' @return invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' tessiflow::tessiflow_enable()
#' }
tessiflow_enable <- function() {
  schedule <- if (.Platform$OS.type == "windows") {
    schedule_schtasks
  } else {
    schedule_crontab
  }

  schedule(
    script_expr(tessiflow::tessiflow_start()), "tessiflow"
  )

  invisible()
}

#' @describeIn tessiflow_enable Unschedule the tessiflow job that runs tessiflow_run.
#' @export
tessiflow_disable <- function() {
  unschedule <- if (.Platform$OS.type == "windows") {
    unschedule_schtasks
  } else {
    unschedule_crontab
  }

  unschedule("tessiflow")

  invisible()
}

#' tessiflow_pid_lock/unlock
#'
#' Files to manage `tessiflow.pid` file to ensure only one process is running at a time
#'
#' @param flows_log_dir character directory name
#'
#' @return nothing invisibly
#' @describeIn tessiflow_pid_lockunlock Create `tessiflow.pid` file
tessiflow_pid_lock <- function(flows_log_dir) {
  pid_file <- file.path(flows_log_dir, "tessiflow.pid")
  if (file.exists(pid_file)) {
    stop("Found running tessiflow process, cowardly refusing to start another.")
  }

  write(Sys.getpid(), pid_file)
}

#' @describeIn tessiflow_pid_lockunlock Remove `tessiflow.pid` file
tessiflow_pid_unlock <- function(flows_log_dir) {
  pid_file <- file.path(flows_log_dir, "tessiflow.pid")
  if (!file.exists(pid_file)) {
    return(invisible())
  }

  pid <- readLines(pid_file)
  if (Sys.getpid() != pid) {
    stop("tessiflow.pid does not match current pid, cowardly refusing to delete old lock file")
  }

  file.remove(pid_file)
  invisible()
}
