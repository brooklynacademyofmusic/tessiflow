#' job_log_write
#'
#' Simple logfile writer, with filename determined by the workflow. Timestamp and job name are prepended to each line before writing.
#'
#' @param flow_name character workflow name
#' @param job_name character job name
#' @param lines character vector of lines to write to the file
#' @param console boolean whether to echo to the console, default is FALSE
#' @importFrom checkmate assert_character
#' @importFrom lubridate today
#' @importFrom utils zip
#'
job_log_write <- function(flow_name, job_name, lines, console = FALSE) {
  assert_flow_job_name(flow_name, job_name)
  assert_character(lines)

  # add time and job info
  lines <- paste("[", Sys.time(), ":", job_name, "]", lines)
  filename <- file.path(config::get("tessiflow.log"), paste0(flow_name, ".log"))

  # rotate logs if over 1M
  if (file.exists(filename) && file.info(filename)$size > 1024^2) {
    zip_filename <- paste0(gsub(".log", "", filename, fixed = TRUE), "-", today(), ".zip")
    zip(zip_filename, filename, flags = "-j -q") # -j : "junk" the directory structure
    if (file.exists(zip_filename)) file.remove(filename)
  }

  write(lines, filename, sep = "\r\n", append = TRUE)

  if (console) invisible(lapply(lines, message))

  invisible()
}
