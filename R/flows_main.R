#' flows_main
#' main flows loop
#'
#' @return NULL, never
#'

flows_main <- function() {
  flows <- flows_parse()
  
}

#' job_maybe_run
#' 
#' check to see if it's time to run a job and call the job runner if it is.
#' the job will be run if following conditions are all true:
#' - `runs-on` must match the computer name as listed in `Sys.info()["nodename"]`
#' - `if` must be true in the current context
#' - `needs` must have finished running since the last time this ran (with retval = 0 unless `if` evaluates to true)
#' - the most recent run in `scheduled_runs` is after the last actual run time for this job
#'
#' @param job a named list of job parameters, i.e. one row of the flows table 
#'
#' @return invisibly
#' @importFrom utils tail
#'
job_maybe_run <- function(job) {

  last_run = flows_log_get_last_run(job$flow_name,job$job_name)
  if(nrow(last_run) == 0)
    last_run$end_time = flows_log_get_create_time()
  
  # check runs-on
  if(!is.null(job_runs_on <- job$jobs[[1]]$`runs-on`))
    check_runs_on = job_runs_on == Sys.info()["nodename"]
  
  # check if
  if(!is.null(job_if <- job$jobs[[1]]$`if`))
    check_if = eval(rlang::parse_expr(as.character(job_if)))
  
  # check needs
  if(!is.null(job_needs <- job$jobs[[1]]$needs)) {
    dependencies = flows_log_get_last_run(job$flow_name,job_needs)
    check_needs = all(dependencies$end_time > last_run$end_time) && 
      (all(dependencies$retval == 0) || exists("check_if") && check_if) &&
      nrow(dependencies) == length(job_needs)
  }
  
  # check schedule
  if(length(job_scheduled_runs <- job$scheduled_runs[[1]]) > 0)
    check_schedule = job_scheduled_runs %>% lapply(purrr::keep,~.<now()) %>% lapply(tail,1) >
      last_run$end_time

  if((!exists("check_runs_on") || check_runs_on) &&
     (!exists("check_if") || check_if) &&
     (!exists("check_needs") || check_needs) &&
     (!exists("check_schedule") || !is.na(check_schedule) && check_schedule)) 
    job_run(job)
  
}


