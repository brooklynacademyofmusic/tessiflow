
#' tessiflow_start
#'
#' Start a background tessiflow process
#'
#' @return the process object
#' @export
tessiflow_start <- function() {
  flows_log_dir = config::get("tessiflow.log")
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir)) {
    stop("Please set the tessiflow.log config option to a directory where log files will be stored")
  }
  
  # called for error-checking side-effects
  flows_parse()
  
  tree <- ps::ps_find_tree("tessiflow_daemon")
  if(length(tree)>0) {
    stop("Running tessiflow process found, cowardly refusing to start another.")
  } 
  
  logfile <- file.path(flows_log_dir, "tessiflow.log")
  
 # rotate old logfile
  if (file.exists(logfile)) {
    zip_filename <- paste0(gsub(".log", "", logfile, fixed = TRUE), "-", as.Date(file.info(logfile)$ctime), ".zip")
    zip(zip_filename, logfile, flags = "-j -q") # -j : "junk" the directory structure
    if (file.exists(zip_filename)) file.remove(logfile)
  }

  tessiflow$r <- callr::r_bg(eval,
    supervise = FALSE,
    args = list(quote({
      tessiflow::flows_main()
    })),
    env = c("tessiflow_daemon"="YES"),
    stdout = logfile,
    stderr = "2>&1",
    poll_connection = FALSE
  )
}

#' tessiflow_stop
#'
#' @importFrom ps ps_kill_tree
tessiflow_stop <- function() {
  if (!is.null(tessiflow$r)) {
    tessiflow$r$kill()
    tessiflow$r <- NULL
  } else {
    ps::ps_kill_tree("tessiflow_daemon")
  }
}

tessiflow_enable <- function() {
  system(paste(
    "schtasks", "/create",
    "/tn", "tessiflow",
    "/sc", "minute",
    "/mo", 1,
    "/tr", shQuote(paste(
      Sys.which("Rscript"),
      "-e", "tessiflow::tessiflow_start()",
      "--vanilla"
    ))
  ))
}

tessiflow_disable <- function() {
  system(paste("schtasks", "/delete", "/tn", "tessiflow"))
}

tessiflow_job_start <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)
  
  tree <- ps::ps_find_tree("tessiflow")
  if(length(tree)==0) {
    stop("No running tessiflow process found, can't start job")
  } 
  
  conns <- rbindlist(lapply(tree,ps::ps_connections))
  socket <- socketConnection(port = conns$lport[[1]])
  
  writeLines(deparse(rlang::expr(job_start(!!flow_name, !!job_name))))
}
