# Server ------------------------------------------------------------------

#' job_force_stop
#'
#' @param flow_name 
#' @param job_name 
#' @rdname api
#' @export
job_force_stop <- function(flow_name, job_name) {
  . <- start_time <- NULL
  assert_flow_job_name(flow_name, job_name)
  
  job <- setDT(flows_log_get_last_run(flow_name, job_name))
  
  sqlite_upsert("jobs", job[,.(flow_name, job_name, start_time, status = "Forced stop")])
  
  flows_log_get_last_run(flow_name,job_name)
}

#' @describeIn api job_force_start
#' @export
job_force_start <- function(flow_name, job_name) {
  . <- start_time <- NULL
  assert_flow_job_name(flow_name, job_name)
  
  job <- setDT(flows_log_get_last_run(flow_name, job_name))
  
  sqlite_upsert("jobs", job[,.(flow_name, job_name, start_time, status = "Forced start")])
  
  flows_log_get_last_run(flow_name,job_name)
}

#' @describeIn api flows_get
#' @export
flows_get <- function() {
  flows <- flows_parse()
  flows_log_get_last_run(flows$flow_name, flows$job_name)
}

#' api_start
#' 
#' Start the plumber API for tessiflow and override the working directory
#' @importFrom plumber plumb pr_hook pr_run
api_start <- function(working_dir = getwd(), docs = FALSE, debug = FALSE) {
  force(working_dir)
  
  setwd(system.file("api",package="tessiflow"))
  pr <- plumb("entrypoint.R")
  setwd(working_dir)
  
  pr %>% pr_run(host = "127.0.0.1", port = config::get("tessiflow.port"),docs = docs, debug = debug)
}


# Client ------------------------------------------------------------------

#' tessiflow_job_start
#'
#' @param flow_name 
#' @param job_name 
#' @importFrom httr POST modify_url
#' @export
tessiflow_job_start <- function(flow_name, job_name, hostname = "127.0.0.1", port = config::get("tessiflow.port")) {
  POST(modify_url("http://",
                  hostname = hostname,
                  port = port,
                  path = "job_start",
                  query = list("flow_name" = flow_name,
                               "job_name" = job_name)))
}

#' tessiflow_job_stop
#'
#' @param flow_name 
#' @param job_name 
#' @importFrom httr POST
#' @export
tessiflow_job_stop <- function(flow_name, job_name, hostname = "localhost", port = config::get("tessiflow.port")) {
  POST(modify_url("http://",
                  hostname = hostname,
                  port = port,
                  path = "job_stop",
                  query = list("flow_name" = flow_name,
                               "job_name" = job_name)))
}

#' tessiflow_flows_get
#'
#' @param flow_name 
#' @param job_name 
#' @importFrom httr GET
#' @export
tessiflow_flows_get <- function(hostname = "localhost", port = config::get("tessiflow.port")) {
  GET(modify_url("http://",
                  hostname = hostname,
                  port = port,
                  path = "flows_get"))
}
