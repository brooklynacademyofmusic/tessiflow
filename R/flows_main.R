#' flows_main
#'
#' main flows loop
#'
#' @importFrom rlang try_fetch
#' @export
#'
#' @return NULL, never!
flows_main <- function() {
  data.table::setDTthreads(1) # Don't multithread this loop to avoid database/process locks
  status <- on.schedule <- NULL

  while (!all(tessiflow$flows$status == "Finished")) {
    try_fetch(
      {
        flows_auto_refresh()

        if ("Waiting" %in% tessiflow$flows$status) {
          tessiflow$flows[status == "Waiting", apply(.SD, 1, function(.) {
            job_maybe_start(.$flow_name, .$job_name)
          })]
        }

        if ("Running" %in% tessiflow$flows$status) {
          tessiflow$flows[status == "Running", apply(.SD, 1, function(.) {
            job_poll(.$flow_name, .$job_name)
          })]
        }

        if ("Finished" %in% tessiflow$flows$status) {
          tessiflow$flows[
            !status %in% c("Waiting", "Running") & sapply(on.schedule, length) > 0,
            apply(.SD, 1, function(.) {
              job_reset(.$flow_name, .$job_name)
            })
          ]
        }

        Sys.sleep(1)
      },
      error = error_calling_handler
    )
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

  assert_flow_job_name(.flow_name, .job_name)

  job <- tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, ]

  if (nrow(job) == 0) {
    stop(paste("No matching job found for", .flow_name, "/", .job_name))
  }

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
  flow_name <- job_name <- . <- start_time <- NULL

  assert_flow_job_name(.flow_name, .job_name)

  assert_list(data)

  local_job <- tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, .(flow_name, job_name, start_time)][, (names(data)) := data]

  sqlite_upsert("jobs", local_job)

  tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, (names(data)) := data]

  invisible()
}
