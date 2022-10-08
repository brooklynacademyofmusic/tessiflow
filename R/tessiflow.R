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
    cat("Starting tessiflow scheduler ...\n")
    flows_main()
  }, callback = log_callback, package = TRUE, stderr = "2>&1")

  invisible()
}

#' @describeIn tessiflow_run Call tessiflow_run from the `R_HOME` directory, and pause on error
#' @export
tessiflow_start <- function() {

  tryCatch({
    local_dir(Sys.getenv("R_USER"))
    tessiflow_run()
    },
    error = function(e) {
      error_print(e)
      countdown(30)
    }
  )
}



countdown <- function(sec) {
  while(sec>0) {
    cat(paste0("Pausing for ",sec," seconds...\r"))
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

#' tessiflow_job_start
#'
#' Starts a tessiflow job identified by `flow_name` and `job_name`
#'
#' @param flow_name string flow name
#' @param job_name sting job name
#'
#' @return invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' tessiflow::tessiflow_job_start("Workflow A", "Job 1")
#' tessiflow::tessiflow_job_stop("Workflow A", "Job 1")
#' }
tessiflow_job_start <- function(flow_name, job_name) {
  last_run <- flows_log_get_last_run(flow_name, job_name)
  if(last_run$status[[1]] == "Running") 
    warning(paste0(flow_name,"/",job_name,"is already running."))
  tessiflow_run_command(flow_name, job_name, "job_start")
}


#' @describeIn tessiflow_job_start Stops a tessiflow job identified by `flow_name` and `job_name`
#' @export
tessiflow_job_stop <- function(flow_name, job_name) {
  tessiflow_run_command(flow_name, job_name, "job_stop")
}

#' @describeIn tessiflow_job_start Refreshes the tessiflow flows configuration from local yml files
#' @export
tessiflow_refresh <- function() {
  flows_refresh() #update the local flows configuration too
  tessiflow_run_command(command="flows_refresh")
}

#' @describeIn tessiflow_job_start Template function for executing commands on the main tessiflow instance
#' @param command string function to be called with `flow_name` and `job_name` as parameters
#' @importFrom stats na.omit
tessiflow_run_command <- function(flow_name=NULL, job_name=NULL, command) {
  if(!is.null(flow_name) && !is.null(job_name))
    assert_flow_job_name(flow_name, job_name)

  if (!file.exists(file.path(config::get("tessiflow.log"), "tessiflow.pid"))) {
    stop(paste(
      "Tessiflow process does not appear to be running,",
      file.path(config::get("tessiflow.log"), "tessiflow.pid"), "is missing."
    ))
  }

  port <- config::get("tessiflow.port")

  tryCatch(socket <- socketConnection(port = port),
    error = function(e) {
      rlang::cnd_message(e)
      stop(paste("Can't connect to tessiflow instance at port", port))
    }
  )

  writeLines(deparse(rlang::call2(command, flow_name = flow_name, job_name = job_name)), socket)

  close(socket)

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
