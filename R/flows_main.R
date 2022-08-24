#' flows_main
#'
#' main flows loop
#'
#' @importFrom stats runif
#' @export
#'
#' @return NULL, never!
flows_main <- function() {
  status <- on.schedule <- NULL

  tessiflow$flows <- flows_parse()

  server <- serverSocket(ceiling(runif(1, 2^10, 2^16)))

  while (! all(tessiflow$flows$status=="Finished")) {
    if ("Waiting" %in% tessiflow$flows$status) {
      tessiflow$flows[status == "Waiting", apply(.SD, 1, function(.) {
        job_maybe_start_resilient(.$flow_name, .$job_name)
      })]
    }

    if ("Running" %in% tessiflow$flows$status) {
      tessiflow$flows[status == "Running", apply(.SD, 1, function(.) {
        job_poll_resilient(.$flow_name, .$job_name)
      })]
    }

    if ("Finished" %in% tessiflow$flows$status) {
      tessiflow$flows[
        status == "Finished" & sapply(on.schedule, length) > 0,
        apply(.SD, 1, function(.) {
          job_reset_resilient(.$flow_name, .$job_name)
        })]
    }

    flows_main_read_server(server)

    Sys.sleep(1)
  }

  close(server)
}

flows_main_read_server <- function(server) {
  allowed_calls <- c("job_start", "job_stop")

  socket <- try(socketAccept(server, timeout = 1), silent = TRUE)
  if (!"try-error" %in% class(socket)) {
    input <- readLines(socket, n = 1)
    close(socket)
    
    if (!test_parse(input)) {
      message(paste0("Can't parse '", input, "' from input stream."))
    } else if (length(input) > 0) {
      expr <- rlang::parse_expr(input)
      if (!is.call(expr) || !rlang::call_name(expr) %in% allowed_calls) {
        return(message(paste0(
          "Expression must be one of ",
          paste(allowed_calls, collapse = ", "),
          ", got ", input
        )))
      }

      tryCatch(eval(rlang::parse_expr(input)), error = print)
    }
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
  flow_name <- job_name <- NULL

  assert_flow_job_name(.flow_name, .job_name)

  assert_list(data)

  tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, (names(data)) := data]

  flows_log_upsert("jobs", tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, ])
  invisible()
}
