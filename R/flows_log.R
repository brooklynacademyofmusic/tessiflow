#' flows_log_open
#'
#' @param flows_log_dir directory where the SQLite database is stored
#'
#' @return invisible
#'
flows_log_open <- function(flows_log_dir = config::get("tessiflow.log")) {
  if (is.null(flows_log_dir) || !dir.exists(flows_log_dir))
    stop("Please set the tessiflow.log config option to a directory where log files will be stored")
  
  if (is.null(tessiflow$db)) {
      tessiflow$db <- DBI::dbConnect(RSQLite::SQLite(), file.path(flows_log_dir,"flows.sqlite"))
  }
  
  if (!DBI::dbExistsTable(tessiflow$db,"jobs"))
    DBI::dbCreateTable(tessiflow$db,"jobs",fields = c(flow_name = "character",
                                                      job_name = "character",
                                                      status = "character",
                                                      start_time = "double",
                                                      end_time = "double",
                                                      retval = "integer"))
  
  invisible()
}

tessiflow <- new.env()

#' @describeIn flows_log_open Tear down the SQL connection
flows_log_close <- function() {
  if(!is.null(tessiflow$db))
    DBI::dbDisconnect(tessiflow$db)
  tessiflow$db <- NULL
}

db <- new.env(parent = emptyenv())

#' flows_log_write
#'
#' @param table string, right now only `jobs` is valid
#' @param data data.frame of data to write
#'
#' @return invisibly
#' @importFrom checkmate assert_choice assert_names assert_class
#'
flows_log_write <- function(table = "jobs",data) {
  assert_choice(table,"jobs")
  flows_log_open()
  assert_names(names(data),subset.of=DBI::dbListFields(tessiflow$db,"jobs"),
               .var.name = names(rlang::enexpr(data)))
  assert_class(data,"data.frame")
  
  DBI::dbAppendTable(tessiflow$db,table,data)
}

#' flows_log_get_last_run
#'
#' @param flows character vector of flows
#' @param jobs character vector of jobs
#'
#' @return matching rows from the log database for the last run of each flow, job pair
#' @importFrom checkmate assert_character
#' @importFrom dplyr tbl inner_join group_by slice_max collect
#'
flows_log_get_last_run <- function(flows,jobs) {
  flow_name <- job_name <- start_time <- NULL
  
  assert_character(flows,any.missing = FALSE)
  assert_character(jobs,any.missing = FALSE)
  flows_log_open()
  
  query_tbl = data.frame(flow_name = flows,job_name = jobs)
  
  tbl(tessiflow$db,"jobs") %>% 
    inner_join(query_tbl,by=c("flow_name","job_name"),copy = TRUE) %>%
    group_by(flow_name,job_name) %>%
    slice_max(start_time,1) %>% collect
}

#' flows_log_get_create_time
#'
#' @return creation time of the log database
#'
flows_log_get_create_time <- function() {
  flows_log_open()
  
  file.info(DBI::dbGetInfo(tessiflow$db)$dbname)$ctime
}