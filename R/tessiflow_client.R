#' tessiflow_job_start
#'
#' Starts a tessiflow job identified by `flow_name` and `job_name`
#'
#' @param flow_name string flow name
#' @param job_name sting job name
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed on to the tessiflow daemon
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
  if (last_run$status[[1]] == "Running") {
    warning(paste0(flow_name, "/", job_name, "is already running."))
  }
  tessiflow_run_command("job_start", !!!c(flow_name = flow_name, job_name = job_name))
}


#' @describeIn tessiflow_job_start Stops a tessiflow job identified by `flow_name` and `job_name`
#' @export
tessiflow_job_stop <- function(flow_name, job_name) {
  tessiflow_run_command("job_stop", !!!c(flow_name = flow_name, job_name = job_name))
}

#' @describeIn tessiflow_job_start Refreshes the tessiflow flows configuration from local yml files
#' @export
tessiflow_refresh <- function() {
  flows_refresh() # update the local flows configuration too
  tessiflow_run_command(command = "flows_refresh")
}

#' @describeIn tessiflow_job_start Template function for executing commands on the main tessiflow instance
#' @param command string function to be called with arguments `...`
#' @importFrom rlang list2
tessiflow_run_command <- function(command, ...) {
  args <- list2(...)
  
  if (!is.null(args$flow_name) || !is.null(args$job_name)) {
    assert_flow_job_name(args$flow_name, args$job_name)
  }
  
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
  
  writeLines(deparse(rlang::call2(command, !!!args)), socket)
  
  close(socket)
  
  invisible()
}