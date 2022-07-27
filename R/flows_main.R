#' flows_main
#' main flows loop
#'
#' @return NULL, never!
#'

flows_main <- function() {
  status <- on.schedule <- NULL
  
  tessiflow$flows <- flows_parse()

  while (!all(tessiflow$flows$status == "Finished")) {
    
    if("Waiting" %in% tessiflow$flows$status)
      tessiflow$flows[status == "Waiting", apply(.SD, 1, function(.) {
        job_maybe_start_resilient(.$flow_name, .$job_name)
      })]

    if("Running" %in% tessiflow$flows$status)
      tessiflow$flows[status == "Running", apply(.SD, 1, function(.) {
        job_poll_resilient(.$flow_name, .$job_name)
      })]
    
    if("Finished" %in% tessiflow$flows$status)
      tessiflow$flows[status == "Finished" & sapply(on.schedule,length)>0,
                      `:=`(status = "Waiting",
                           scheduled_runs = lapply(on.schedule, lapply, parse_cron))]

    Sys.sleep(1)
  }
  
}



#' flows_get_job
#'
#' Lookup the flow row in the local data.table and return it as a list
#'
#' @param .flow_name string workflow name
#' @param .job_name string job name
#' @importFrom purrr map
#'
#' @return list, one row of the flows table
#'
flows_get_job <- function(.flow_name, .job_name) {
  flow_name <- job_name <- NULL
  
  assert_character(.flow_name, len = 1)
  assert_character(.job_name, len = 1)
  job <- tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, ]
  
  if(nrow(job)==0)
    stop(paste("No matching job found for",.flow_name,"/",.job_name))
  
  job %>% map(unlist, recursive = FALSE)
}

#' flows_update_job
#'
#' @param .flow_name string workflow name
#' @param .job_name string job name
#' @param data list of data to update the flow with
#' @importFrom checkmate assert_list
#' @return invisibly
flows_update_job <- function(.flow_name, .job_name, data) {
  flow_name <- job_name <- NULL
  
  assert_character(.flow_name, len = 1)
  assert_character(.job_name, len = 1)
  assert_list(data)

  tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, (names(data)) := data]

  flows_log_upsert("jobs", tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, ])
  invisible()
}
