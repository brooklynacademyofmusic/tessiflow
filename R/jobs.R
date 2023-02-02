
#' job_maybe_start
#'
#' check to see if it's time to start a job and call the job runner if it is.
#' the job will be started if following conditions are all true:
#' - `runs-on` must match the computer name as listed in `Sys.info()["nodename"]`
#' - `if` must be true in the current context
#' - `needs` must have finished running since the last time this ran (with return value = 0 unless `if` evaluates to true)
#' - the most recent run in `scheduled_runs` is after the last actual run time for this job
#'
#' @param flow_name string workflow name
#' @param job_name string job name
#'
#' @return invisibly
#' @importFrom utils tail
#'
job_maybe_start <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  job <- flows_get_job(flow_name, job_name)

  last_run <- flows_log_get_last_run(job$flow_name, job$job_name)
  if (nrow(last_run) == 0) {
    last_run <- rbind(last_run, list(end_time = as.double(flows_log_get_create_time())))
  }

  # check runs-on
  check_runs_on <- job$`runs-on` %||% NA == Sys.info()["nodename"] # T/F/NA
  # check if
  check_if <- eval(rlang::parse_expr(as.character(job$`if` %||% NA))) # T/F/NA
  # check needs
  dependencies <- flows_log_get_last_run(job$flow_name, job$needs %||% "")

  check_needs <- !any(is.na(dependencies$end_time)) && 
    all(dependencies$end_time > last_run$end_time) &&
    (all(dependencies$retval == 0) || !is.na(check_if) && check_if) &&
    nrow(dependencies) == length(job$needs)
  
  # check schedule
  check_schedule <- any(job$scheduled_runs[[1]] %>% unlist() %>% purrr::keep(~ . < now()) %||% NA >
    last_run$end_time) # T/F/NA

  if (
    (is.na(check_runs_on) || check_runs_on) &&
      (is.na(check_if) || check_if) &&
      (is.na(check_needs) || check_needs) &&
      (is.na(check_schedule) || check_schedule)) {
    job_start(flow_name, job_name)
  }
}

#' job_start
#'
#' Run the job in a `callr` process
#'
#' @param flow_name string workflow name
#' @param job_name string job name
#'
#' @importFrom callr r_session r_session_options
#' @importFrom lubridate now
#'
#' @return invisibly
#'
job_start <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  # spin up a callr process
  job <- flows_get_job(flow_name, job_name)

  if (is.null(job$r_session) || job$r_session[[1]]$get_state() == "finished") {
    r_session <- r_session$new(options = r_session_options(
      stdout = "|",
      stderr = "|",
      env = unlist(job$env) %||% callr::rcmd_safe_env()
    ), wait = TRUE, wait_timeout = 30000)
    
    flows_update_job(
      flow_name, job_name,
      list(
        r_session = list(r_session),
        pid = r_session$get_pid(),
        step = 0,
        start_time = now(),
        status = "Running"
      )
    )

    job_log_write(flow_name, job_name, paste("Starting job, pid:", r_session$get_pid()), console = TRUE)
  } else {
    warning(paste("Job", job$flow_name, "/", job$job_name, "has already been started!"))
  }

  return(invisible())
}

#' @describeIn job_start Run the next step in a job
job_step <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  job <- flows_get_job(flow_name, job_name)

  if (job$step == length(job$steps)) {
    return(job_finalize(flow_name, job_name))
  }

  step <- job$step + 1
  current_step <- job$steps[[job$step + 1]]

  job$r_session[[1]]$call(job_make_remote_fun(c(current_step$env, job$env),
                                                          as.character(current_step$`if`),
                                                          as.character(current_step$run),
                                                          shell = current_step$shell %||% "callr"
  ))
  
  flows_update_job(
    flow_name, job_name,
    list(step = step)
  )

  if (is.null(job$r_session) || job$r_session[[1]]$get_state() == "finished") {
    warning(paste("Job", flow_name, "/", job_name, "has no running R session."))
    job_log_write(flow_name, job_name, paste("No running R session, skipping", step, ":", current_step$name), console = TRUE)
  } else {
    job_log_write(flow_name, job_name, paste("Beginning step", step, ":", current_step$name), console = TRUE)

  }

  return(invisible())
}

#' job_make_remote_fun
#'
#' Creates an function that sets local environment variables `env`, does nothing if `if_expr` evaluates
#' to false and otherwise evaluates `run_expr` transparently. If shell is not `callr` then the `run_expr` gets
#' wrapped in a `system()` command to execute the shell.
#'
#' @param env_vars named list of environment variables
#' @param if_expr deparsed if expression
#' @param run_expr deparsed run expression
#' @param shell string setting the shell, default is `callr`
#'
#' @return function
job_make_remote_fun <- function(env_vars = list(), if_expr = NULL, run_expr = NULL, shell = "callr") {
  assert_character(if_expr, max.len = 1, null.ok = TRUE)
  assert_character(run_expr, max.len = 1, null.ok = TRUE)
  assert_character(shell, max.len = 1, null.ok = TRUE)
  assert_list(env_vars, names = "named", types = "character", null.ok = TRUE)

  if (length(run_expr) == 0) {
    return()
  }

  if (length(if_expr) == 0) {
    if_expr <- "TRUE"
  }

  run_expr <- if (length(shell) != 0 && shell != "callr") {
    if (!grepl("{0}", shell, fixed = TRUE)) shell <- paste(shell, "{0}")
    rlang::expr(system(!!gsub("{0}", shQuote(run_expr), shell, fixed = TRUE), intern = TRUE))
  } else {
    rlang::parse_exprs(run_expr)
  }

  as.function(list(rlang::expr(
    withCallingHandlers(
      withr::with_envvar(!!env_vars,{
        if (!(!!rlang::parse_expr(if_expr))) {
          message("'if' expression is not true, skipping")
          return(invisible(NULL))
        } else {
          !!!run_expr
        }
      }),
      error=rlang::entrace,
      warning=rlang::entrace,
      message=rlang::entrace
  ))))

}

#' @param error error condition object
#' @importFrom checkmate assert_class assert_character
#' @importFrom cli ansi_strip
#' @describeIn job_start Updates flows table and database, writes to log, and finalizes running session on error
job_on_error <- function(flow_name, job_name, error) {
  assert_class(error, "error")
  assert_flow_job_name(flow_name, job_name)

  job_finalize(flow_name, job_name)
  
  flows_update_job(flow_name, job_name, list(retval = 1))
  job_log_write(flow_name, job_name, error$message, console = TRUE)
  job_log_write(flow_name, job_name, cli::ansi_strip(format(error$trace)))
  
  error$flow_name <- flow_name
  error$job_name <- job_name
  error_handler(error)
  invisible()
}

#' @describeIn job_start Read `stdout` and `stderr` from the process and write to log. When ready, call job_step
job_poll <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  job <- flows_get_job(flow_name, job_name)

  if (is.null(job$r_session) || job$r_session[[1]]$get_state() == "finished") {
    warning(paste("Job", flow_name, "/", job_name, "has no running R session."))
    return(invisible())
  }

    io_state <- job$r_session[[1]]$poll_io(1)

    io_names <- names(which(io_state == "ready"))
    io_funs <- c(
      output = quote(read_output_lines()),
      error = quote(read_error_lines()),
      process = quote(read())
    )[io_names]

    output <- purrr::discard(lapply(io_funs, eval, envir = job$r_session[[1]]), ~ length(.) == 0)
    if (length(output)) {
      output_str <- lapply(output, purrr::imap, ~ paste(.y, ":", .x))
      output_str <- purrr::imap(output_str, ~ paste("[", toupper(.y), "]", .x)) %>% purrr::flatten_chr()
      job_log_write(flow_name, job_name, output_str)
    }
    if ("process" %in% names(output) && !is.null(output[["process"]]$error)) {
      e <- job$r_session[[1]]$run(rlang::last_error,package=T)
      job_on_error(flow_name, job_name, e)
    }
    if (!job$r_session[[1]]$is_alive()) {
      job_finalize(flow_name, job_name)
    }

  if (job$r_session[[1]]$get_state() == "idle") {
    job_step(flow_name, job_name)
  }
}

#' @describeIn job_start Closes R session, writes to log, console and database, and updates `retval`.
job_finalize <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  job <- flows_get_job(flow_name, job_name)

  r_session <- job$r_session

  if (!is.na(job$retval) && job$retval != 0) {
    warning(paste(flow_name, "/", job_name, "Errored, returned value:", job$retval))
  } else {
    job$retval <- 0
  }

  if (is.null(r_session) || r_session[[1]]$get_state() == "finished") {
    warning(paste("Job", flow_name, "/", job_name, "has no running R session."))
  } else {
    job$r_session[[1]]$close()
  }
  
  flows_update_job(
    flow_name, job_name,
    list(
      r_session = list(NULL),
      retval = job$retval,
      end_time = now(),
      status = "Finished"
    )
  )
  
  job_log_write(flow_name, job_name, paste("Finalizing job, pid:", job$pid), console = TRUE)
}

#' @describeIn job_start Resets job for next run, updates flows table but NOT database, writes to log
job_reset <- function(flow_name, job_name) {
  on.schedule <- NULL
  assert_flow_job_name(flow_name, job_name)

  tessiflow$flows[
    eval(rlang::expr(flow_name == !!flow_name & job_name == !!job_name)),
    `:=`(
      status = "Waiting",
      retval = NA_integer_,
      r_session = list(NULL),
      pid = NA_integer_,
      step = NA_integer_,
      scheduled_runs = lapply(on.schedule, lapply, parse_cron),
      start_time = as.POSIXct(NA),
      end_time = as.POSIXct(NA)
    )
  ]

  job_log_write(flow_name, job_name, paste("Resetting job"), console = TRUE)
}

#' @describeIn job_start Stops job, updates flows table and database, writes to log
job_stop <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  job <- flows_get_job(flow_name, job_name)

  job_finalize(flow_name, job_name)

  flows_update_job(
    flow_name, job_name,
    list(
      status = "Stopped"
    )
  )
  
  job_log_write(flow_name, job_name, paste("Stopping job, pid:", job$pid), console = TRUE)
  
}

job_maybe_start_resilient <- error_handler_factory(job_maybe_start)
job_poll_resilient <- error_handler_factory(job_poll)
job_reset_resilient <- error_handler_factory(job_reset)
