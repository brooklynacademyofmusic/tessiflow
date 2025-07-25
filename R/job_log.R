#' job_log_write
#'
#' Simple log file writer, with file name determined by the workflow. Timestamp and job name are prepended to each line before writing.
#'
#' @param flow_name character workflow name
#' @param job_name character job name
#' @param lines character vector of lines to write to the file
#' @param console boolean whether to echo to the console, default is FALSE
#' @importFrom checkmate assert_character
#' @importFrom lubridate today
#' @importFrom utils zip
#' @importFrom stats na.omit
job_log_write <- function(flow_name, job_name = NA, lines, console = FALSE) {
  assert_character(lines)

  # add time and job info
  lines <- paste("[", Sys.time(), ":", paste(na.omit(c(flow_name, job_name)), collapse = " / "), "]", lines)
  filename <- file.path(config::get("tessiflow.log"), paste0(flow_name, ".log"))
  log_rotate(filename)

  write(cli::ansi_strip(lines), filename, sep = "\r\n", append = TRUE)

  if (console) invisible(lapply(lines, message))

  invisible()
}

#' log_rotate
#'
#' Rotate log file `filename` into a zip file if its size exceeds `size` bytes
#'
#' @param filename string file name of log file to (potentially) rotate
#' @param size integer size in bytes, default is 1 megabyte
#'
#' @return nothing, invisibly
log_rotate <- function(filename, size = config::get("tessiflow.logsize") %||% 1024^2) {
  zip_filename <- paste0(gsub(".log", "", filename, fixed = TRUE), "-", format(now(),"%Y-%m-%d %H-%M-%S"), ".zip")
  if (file.exists(filename) && file.info(filename)$size > size && !file.exists(zip_filename)) {
    status = zip(zip_filename, filename, flags = "-j -q") # -j : "junk" the directory structure
    if (file.exists(zip_filename) & status == 0) file.remove(filename)
  }

  invisible()
}
