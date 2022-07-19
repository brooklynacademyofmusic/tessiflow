#' flows_main
#' main flows loop
#'
#' @return NULL, never
#'

flows_main <- function() {
  tessiflow$flows <- flows_parse()

  tryCatch(
    {
      while (!all(tessiflow$flows$status == "Finished")) {
        tessiflow$flows[status == "Waiting", apply(.SD, 1, function(.) {
          job_maybe_start(.$flow_name, .$job_name)
        })]

        tessiflow$flows[status == "Running", apply(.SD, 1, function(.) {
          job_poll(.$flow_name, .$job_name)
        })]

        Sys.sleep(1)
      }
    },
    error = error_email
  )
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
  assert_character(.flow_name, len = 1)
  assert_character(.job_name, len = 1)
  as.list(tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, ]) %>%
    map(unlist, recursive = FALSE)
}

#' flows_update_job
#'
#' @param .flow_name string workflow name
#' @param .job_name string job name
#' @param data list of data to update the flow with
#' @importFrom checkmate assert_list
#' @return invisibly
flows_update_job <- function(.flow_name, .job_name, data) {
  assert_character(.flow_name, len = 1)
  assert_character(.job_name, len = 1)
  assert_list(data)

  tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, (names(data)) := data]

  flows_log_upsert("jobs", tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, ])
  invisible()
}
