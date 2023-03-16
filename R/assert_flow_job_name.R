#' check_flow_job_name
#' @title tests for flow and job names
#' @description check, assertion, and test for flow and job name: both are characters of length 1 and both exist in flows. 
#'
#' @param flow_name `character` flow name
#' @param job_name `character` job name
#' 
#' @param .var.name `character` name of the checked object to print in assertions. Defaults to the heuristic implemented in vname.
#' @param add	`AssertCollection` collection to store assertion messages. See `AssertCollection.`
#'
#' @importFrom checkmate makeAssertCollection reportAssertions assert check_character check_names
#' @describeIn check_flow_job_name returns `TRUE` if check is true, character error message if check is false
check_flow_job_name <- function(flow_name, job_name) {
  if (is.null(tessiflow$flows)) {
    tessiflow$flows <- flows_parse()
  }
  
  checks <- makeAssertCollection()
  assert(
    check_character(flow_name, len = 1),
    check_names(flow_name, subset.of = unique(tessiflow$flows$flow_name)),
    combine = "and",
    add = checks
  )
  
  if(!checks$isEmpty()) 
    return(reportAssertions(checks))
  
  assert(
    check_character(job_name, len = 1),
    check_names(job_name, subset.of = tessiflow$flows[base::get("flow_name") == flow_name]$job_name),
    combine = "and",
    add = checks
  )
  
  reportAssertions(checks)
}
 
#' @describeIn check_flow_job_name returns `TRUE` if check is true, throws error if check is false
assert_flow_job_name <- checkmate::makeAssertionFunction(check_flow_job_name)

#' @describeIn check_flow_job_name returns `TRUE` if check is true, `FALSE` otherwise
test_flow_job_name <- checkmate::makeTestFunction(check_flow_job_name)
