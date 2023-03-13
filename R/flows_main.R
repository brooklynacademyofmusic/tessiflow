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
  tessiflow$flows <- flows_parse()
  server <- serverSocket(config::get("tessiflow.port"))

  while (!all(tessiflow$flows$status == "Finished")) {
    try_fetch({
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
          !status %in% c("Waiting","Running") & sapply(on.schedule, length) > 0,
          apply(.SD, 1, function(.) {
            job_reset(.$flow_name, .$job_name)
          })
        ]
      }
  
      flows_main_read_server(server)
  
      Sys.sleep(1)
    }, error = error_calling_handler)
  }

  close(server)
}

#' flows_main_read_server
#'
#' Reads from socket server and (safely) executes commands by checking the call name and converting arguments to strings.
#'
#' @param server Server object created by `base::socketServer`
#'
#' @return result of call
flows_main_read_server <- function(server) {
  allowed_calls <- c("job_start", "job_stop", "flows_refresh")

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

      # Construct safe call
      tryCatch(eval(rlang::call2(
        rlang::call_name(expr),
        !!!lapply(rlang::call_args(expr), as.character)
      )),
      error = print
      )
    }
  }
}

#' flows_refresh
#'
#' Update flows data.table with new data from yml flows
#'
#' @return updated data.table
#' @importFrom dplyr anti_join
flows_refresh <- function() {
  flows <- flows_parse()

  new_flows <- anti_join(flows, tessiflow$flows, by = c("flow_name", "job_name"))

  update_columns <- intersect(
    c("env", "on.schedule", "runs-on", "steps", "needs", "if", "scheduled_runs"),
    colnames(flows)
  )

  tessiflow$flows <- rbindlist(list(
    tessiflow$flows[flows, (update_columns) := mget(paste0("i.", update_columns)),
      on = c("flow_name", "job_name")
    ],
    new_flows
  ),
  fill = TRUE
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
  
  local_job <- tessiflow$flows[flow_name == .flow_name &
                           job_name == .job_name, ][,(names(data)) := data]

  sqlite_upsert("jobs", local_job)

  tessiflow$flows[flow_name == .flow_name &
    job_name == .job_name, (names(data)) := data]

  invisible()
}
