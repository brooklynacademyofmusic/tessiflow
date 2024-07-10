# Server ------------------------------------------------------------------
#' @name api_server
#' @title Tessiflow API server functions
#' @param flow_name character workflow name
#' @param job_name character job name
{}
#' @describeIn api_server internal api function for forced starting a job
api_job_start <- function(flow_name, job_name) {
  . <- start_time <- NULL
  flows_auto_refresh()
  assert_flow_job_name(flow_name, job_name)
  
  sqlite_upsert("jobs", data.table(flow_name = flow_name, job_name = job_name, 
                                   start_time = now(), end_time = now(), status = "Forced start"))
  
  flows_log_get_last_run(flow_name,job_name)
}

#' @describeIn api_server internal api function for forced stopping a job
api_job_stop <- function(flow_name, job_name) {
  . <- start_time <- NULL
  flows_auto_refresh()
  assert_flow_job_name(flow_name, job_name)
  
  sqlite_upsert("jobs", data.table(flow_name = flow_name, job_name = job_name, 
                                   start_time = now(), end_time = now(), status = "Forced stop"))
  
  flows_log_get_last_run(flow_name,job_name)
}

#' @describeIn api_server internal api function for getting last run of jobs
api_flows_get <- function() {
  flows <- flows_parse()
  flows_log_get_last_run(flows$flow_name, flows$job_name)
}

#' api_start
#' 
#' Start the plumber API for tessiflow and override the working directory and some `plumber` startup options
#'
#' @importFrom plumber plumb pr_hook pr_run
#' @param working_dir character, working directory to start the API server in
#' @param docs boolean, passed on to `plumber::pr_run`
#' @param debug boolean, passed on to `plumber::pr_run`
#'
api_start <- function(working_dir = getwd(), docs = FALSE, debug = FALSE) {
  force(working_dir)
  
  setwd(system.file("api",package="tessiflow"))
  pr <- plumb("entrypoint.R")
  setwd(working_dir)
  
  pr %>% pr_run(host = "127.0.0.1", port = config::get("tessiflow.port"),docs = docs, debug = debug)
}


# Client ------------------------------------------------------------------

#' @title Tessiflow API
#' @name api
#' @description The Tessiflow API makes it possible to communicate with a running tessiflow instance
#' @param hostname hostname of tessiflow server, defaults to `localhost`
#' @param port port of tessiflow server, defaults to `tessiflow.port` config value
#' @param flow_name character workflow name
#' @param job_name character job name
{}

#' @importFrom httr POST modify_url content
#' @describeIn api start job on the tessiflow server
#' @returns * `tessiflow_job_start()`: list of started job data, as returned by [flows_log_get_last_run]
#' @export
tessiflow_job_start <- function(flow_name, job_name, hostname = "localhost", port = config::get("tessiflow.port")) {
  POST(modify_url("http://",
                  hostname = hostname,
                  port = port,
                  path = "job_start",
                  query = list("flow_name" = flow_name,
                               "job_name" = job_name))) %>% content()
}


#' @importFrom httr POST modify_url content
#' @describeIn api stop job on the tessiflow server
#' @returns * `tessiflow_job_stop()`: list of stopped job data, as returned by [flows_log_get_last_run]
#' @export
tessiflow_job_stop <- function(flow_name, job_name, hostname = "localhost", port = config::get("tessiflow.port")) {
  POST(modify_url("http://",
                  hostname = hostname,
                  port = port,
                  path = "job_stop",
                  query = list("flow_name" = flow_name,
                               "job_name" = job_name))) %>% content()
}


#' @importFrom httr GET modify_url content
#' @describeIn api get information on the latest run of each job on the tessiflow server
#' @returns * `tessiflow_flows_get()`: data.table of all latest job data, as returned by [flows_log_get_last_run]
#' @export
tessiflow_flows_get <- function(hostname = "localhost", port = config::get("tessiflow.port")) {
  . <- NULL
  
  date_cols <- c("start_time","end_time")
  
  GET(modify_url("http://",
                  hostname = hostname,
                  port = port,
                  path = "flows_get")) %>% content() %>%
    rbindlist(fill=T) %>% .[,(date_cols) := lapply(.SD, as.POSIXct), .SDcols = date_cols]
}
