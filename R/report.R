
#' tessiflow_report
#'
#'
#' @return nothing
#' @importFrom lubridate now ddays as_datetime
#' @importFrom dplyr tbl filter mutate transmute collect
#' @describeIn tessiflow_report Pull process data from the SQLite database from the last 24hours.
tessiflow_report_load <- function() {
  start_time <- flow_name <- job_name <- retval <- status <- step <- end_time <- `End Time` <- `Start Time` <- NULL

  flows_log_open()

  tasks <- tbl(tessiflow$db, "jobs") %>%
    filter(start_time > !!as.integer(now() - ddays(1))) %>%
    collect() %>%
    transmute(
      `Flow Name` = flow_name,
      `Job Name` = job_name,
      `Status` = ifelse(dplyr::coalesce(retval, 0) != 0 & status != "Running", "Failed", status),
      `Step` = step,
      `Start Time` = as_datetime(start_time, tz = Sys.timezone()),
      `End Time` = as_datetime(end_time, tz = Sys.timezone())
    ) %>%
    mutate(`Elapsed` = ifelse(is.na(`End Time`),
      format(interval_to_period(`Start Time`, now())),
      format(interval_to_period(`Start Time`, `End Time`))
    ))

  if (nrow(tasks) == 0) {
    return(invisible())
  }

  return(tasks)
}

#' @param report data.frame of a report, minimally with a "Status" column
#' @importFrom htmlTable addHtmlTableStyle htmlTable
#' @importFrom dplyr case_when
#' @describeIn tessiflow_report Style and send the flows report in the body of an HTML email
#' @export
tessiflow_report_send <- function(report = tessiflow_report_load()) {
  subject <- "tessiflow report"
  body <- report %>%
    addHtmlTableStyle(col.rgroup = case_when(
      .$Status == "Failed" ~ "#F77",
      .$Status == "Finished" ~ "#7F7",
      .$Status == "Running" ~ "none",
      TRUE ~ "#FF7"
    )) %>%
    htmlTable(rnames = FALSE) %>%
    as.character()

  send_email(subject = subject, body = body)

  invisible()
}

#' tessiflow_report_enable
#'
#' Install `tessiflow.yml` into the `tessiflow.d` directory to run the tessiflow report daily at 9am.
#'
#' @return TRUE if success, FALSE if failure
#' @export
tessiflow_report_enable <- function() {
  file.copy(
    system.file("extdata", "tessiflow.yml", package = "tessiflow"),
    config::get("tessiflow.d")
  )
}

#' interval_to_period
#'
#' converts an interval of start and end times to a pretty printed elapsed time in the format
#' `<hours> H <minutes> M <seconds> S`
#'
#' @param start_time POSIXct
#' @param end_time POSIXct
#' @importFrom  lubridate as.period as.duration interval
#' @return `lubridate::Period` object for pretty printing of time differences
interval_to_period <- function(start_time, end_time) {
  as.period(floor(as.duration(interval(start_time, end_time, tzone = Sys.timezone()))))
}
