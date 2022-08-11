
#' tessiflow_run
#'
#' Wraps flows_main with some logging and environment variables
#' @importFrom withr local_envvar local_output_sink local_message_sink
#' @return nothing, invisibly
#' @export
tessiflow_run <- function() {
  flows_log_dir <- config::get("tessiflow.log")
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir)) {
    stop("Please set the tessiflow.log config option to a directory where log files will be stored")
  }

  # called for error-checking side-effects
  flows_parse()

  tree <- ps::ps_find_tree("tessiflow-daemon")
  if (length(tree) > 0) {
    stop("Found running tessiflow process, cowardly refusing to start another.")
  }

  logfile <- file.path(flows_log_dir, "tessiflow.log")
  log_rotate(logfile)

  local_envvar("tessiflow-daemon" = "YES")
  local_output_sink(logfile, append = TRUE, split = TRUE, .local_envir = environment())
  local_message_sink(logfile, append = TRUE, .local_envir = environment())

  cat(paste("[", Sys.time(), ": tessiflow ]", "Starting tessiflow scheduler ...\n"))

  callr::r(flows_main, stdout = "", stderr = "", package = TRUE)

  invisible()
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

  schedule(rlang::expr({setwd(!!Sys.getenv("R_USER"))
    tessiflow::tessiflow_run()}), "tessiflow")

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
  tessiflow_run_command(flow_name, job_name, "job_start")
}


#' @describeIn tessiflow_job_start Stops a tessiflow job identified by `flow_name` and `job_name`
#' @export
tessiflow_job_stop <- function(flow_name, job_name) {
  tessiflow_run_command(flow_name, job_name, "job_stop")
}

#' @describeIn tessiflow_job_start Template function for executing commands on the main tessiflow instance
#' @param command string function to be called with `flow_name` and `job_name` as parameters
#' @importFrom stats na.omit
tessiflow_run_command <- function(flow_name, job_name, command) {
  assert_flow_job_name(flow_name, job_name)

  conns <- rbindlist(lapply(ps::ps_find_tree("tessiflow-daemon"), ps::ps_connections))
  if (is.null(na.omit(conns$lport))) {
    stop("No running tessiflow process found, can't start job")
  }

  socket <- socketConnection(port = na.omit(conns$lport))

  writeLines(deparse(rlang::call2(command, flow_name = flow_name, job_name = job_name)), socket)

  close(socket)

  invisible()
}
