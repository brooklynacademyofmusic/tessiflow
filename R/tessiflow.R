
#' tessiflow_start
#' 
#' Start a background tessiflow process
#'
#' @param flows_log_dir directory where `tessiflow.log` file and `tessiflow.pipe` input will be stored
#'
#' @return the process object
#' @export
tessiflow_start <- function(flows_log_dir = config::get("tessiflow.log")) {
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir)) {
    stop("Please set the tessiflow.log config option to a directory where log files will be stored")
  }
  
  # called for error-checking side-effects
  flows_parse()
  
  tessiflow$r <- callr::r_bg(eval,
                             supervise = FALSE,
                             args = list(quote({library(tessiflow);flows_main()})),
                             stdout = file.path(flows_log_dir,"tessiflow.log"),
                             stderr = "2>&1",
                             poll_connection = FALSE,
                             stdin = file.path(flows_log_dir,"tessiflow.pipe")
                            )
}

#' tessiflow_stop
#'
#' @importFrom ps ps_kill_tree
tessiflow_stop <- function() {
  if(!is.null(tessiflow$r)) {
    tessiflow$r$kill()
  } else {
    ps::ps_kill_tree("tessiflow")
  }
}

tessiflow_enable <- function() {
  system(paste("schtasks","/create",
               "/tn","tessiflow",
               "/sc","minute",
               "/mo",1,
               "/tr",shQuote(paste(
                 Sys.which("Rscript"),
                 "-e","tessiflow::tessiflow_start()",
                 "--vanilla"))))
}

tessiflow_disable <- function() {
  system(paste("schtasks","/delete","/tn","tessiflow"))
}
