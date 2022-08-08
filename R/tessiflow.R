
#' tessiflow_run
#'
#' Wraps flows_main with some logging and environment variables
#' @importFrom withr local_envvar local_output_sink local_message_sink
#' @return nothing, invisibly
#' @export
tessiflow_run <- function() {
  flows_log_dir = config::get("tessiflow.log")
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir)) {
    stop("Please set the tessiflow.log config option to a directory where log files will be stored")
  }
  
  # called for error-checking side-effects
  flows_parse()
  
  tree <- ps::ps_find_tree("tessiflow-daemon_0")
  if(length(tree)>0) {
    stop("Found running tessiflow process, cowardly refusing to start another.")
  } 
  
  logfile <- file.path(flows_log_dir, "tessiflow.log")
  log_rotate(logfile)
  
  local_envvar("tessiflow-daemon_0"="YES")
  local_output_sink(logfile, append = TRUE, split = TRUE, .local_envir = environment())
  local_message_sink(logfile, append = TRUE, .local_envir = environment())
  
  cat(paste("[", Sys.time(), ": tessiflow ]", "Starting tessiflow scheduler ...\n"))
  
  flows_main()
  
  invisible()
}

#' tessiflow_stop
#'
#' @importFrom ps ps_kill_tree
tessiflow_stop <- function() {
  ps_kill_tree("tessiflow-daemon_0")
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
  schedule <- if(.Platform$OS.type == "windows") {
    schedule_schtasks
  } else {
    schedule_crontab
  }

  schedule(rlang::expr(tessiflow::tessiflow_run()),"tessiflow")
  
  invisible()
}

#' @describeIn tessiflow_enable Unschedule the tessiflow job that runs tessiflow_run.
#' @export
tessiflow_disable <- function() {
  unschedule <- if(.Platform$OS.type == "windows") {
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
#' tessiflow::tessiflow_job_start("Workflow A","Job 1")
#' tessiflow::tessiflow_job_stop("Workflow A","Job 1")
#' }
tessiflow_job_start <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)
  
  tree <- ps::ps_find_tree("tessiflow")
  if(length(tree)==0) {
    stop("No running tessiflow process found, can't start job")
  } 
  
  conns <- rbindlist(lapply(tree,ps::ps_connections))
  socket <- socketConnection(port = conns$lport[[1]])
  
  writeLines(deparse(rlang::expr(job_start(!!flow_name, !!job_name))),socket)
  
  invisible()
}

#' @describeIn tessiflow_job_stop Stops a tessiflow job identified by `flow_name` and `job_name`
#' @export
tessiflow_job_stop <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)
  
  tree <- ps::ps_find_tree("tessiflow")
  if(length(tree)==0) {
    stop("No running tessiflow process found, can't start job")
  } 
  
  conns <- rbindlist(lapply(tree,ps::ps_connections))
  socket <- socketConnection(port = conns$lport[[1]])
  
  writeLines(deparse(rlang::expr(job_stop(!!flow_name, !!job_name))),socket)
  
  invisible()
}
