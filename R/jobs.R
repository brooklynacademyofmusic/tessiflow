#' @name job_
#' @rdname job_start
#' @title job functions
#' @description run, poll, step, read, and finalize tessiflow jobs
#' @param flow_name string workflow name
#' @param job_name string job name
NULL

#' @return invisibly
#' @importFrom utils tail
#' @describeIn job_start check to see if it's time to start a job and call the job runner if it is. 
#' 
#' the job will be started if following conditions are all true:
#' - `runs-on` must match the computer name as listed in `Sys.info()["nodename"]`
#' - `if` must be true in the current context
#' - `needs` must have finished running since the last time this ran (with return value = 0 unless `if` evaluates to true)
#' - the most recent run in `scheduled_runs` is after the last actual run time for this job
job_maybe_start <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  job <- flows_get_job(flow_name, job_name)
  
  last_run <- flows_log_get_last_run(job$flow_name, job$job_name)
  if (nrow(last_run) == 0) {
    last_run <- rbind(last_run, list(end_time = as.double(flows_log_get_create_time())))
  }
  
  # check forced run
  check_force <- last_run$status == "Forced start"
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
  
  if(!is.na(check_force) && check_force) {
    job_log_write(flow_name, job_name, "Force starting job", console = TRUE)
    job_start(flow_name, job_name) 
  } else if (
      (is.na(check_runs_on) || check_runs_on) &&
      (is.na(check_if) || check_if) &&
      (is.na(check_needs) || check_needs) &&
      (is.na(check_schedule) || check_schedule)) {
    job_start(flow_name, job_name)
  }
}


#' @importFrom callr r_session r_session_options
#' @importFrom lubridate now
#'
#' @return invisibly
#' @describeIn job_start run the job in a `callr` process
job_start <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  # spin up a callr process
  job <- flows_get_job(flow_name, job_name)

  if (is.null(job$r_session) || !job$r_session[[1]]$is_alive()) {
    r_session <- r_session$new(options = r_session_options(
      stdout = "|",
      stderr = "|",
      env = unlist(job$env) %||% callr::rcmd_safe_env()
    ), wait = TRUE, wait_timeout = 30000)

    tempdir <- r_session$run(tempdir)

    flows_update_job(
      flow_name, job_name,
      list(
        r_session = list(r_session),
        pid = r_session$get_pid(),
        step = 0,
        tempdir = tempdir,
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

  job_safely_invoke(job,"call",job_make_remote_fun(c(current_step$env, job$env),
    as.character(current_step$`if`),
    as.character(current_step$run),
    shell = current_step$shell %||% "callr"
  ))

  flows_update_job(
    flow_name, job_name,
    list(step = step)
  )
  
  job_log_write(flow_name, job_name, paste("Beginning step", step, ":", current_step$name), console = TRUE)
  
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
    list(rlang::expr(cat(system(!!gsub("{0}", shQuote(run_expr), shell, fixed = TRUE), intern = TRUE))))
  } else {
    rlang::parse_exprs(run_expr)
  }

  as.function(list(rlang::expr(
    withCallingHandlers(
      withr::with_envvar(!!env_vars, {
        if (!(!!rlang::parse_expr(if_expr))) {
          message("'if' expression is not true, skipping")
          return(invisible(NULL))
        } else {
          local({
            options("rlang_trace_top_env" = rlang::current_env())
            !!!run_expr
          })
          invisible(NULL)
        }
      }),
      condition = rlang::entrace
    )
  )))
}

#' @param error error condition object
#' @importFrom checkmate assert_class assert_character
#' @importFrom cli ansi_strip
#' @importFrom rlang cnd_entrace
#' @describeIn job_start Updates flows table and database, writes to log, and finalizes running session on error
job_on_error <- function(flow_name, job_name, error) {
  assert_class(error, "error")
  assert_flow_job_name(flow_name, job_name)
  
  stack <- rlang::trace_back(bottom = 2)$call %>% lapply(rlang::call_name)
  if(any(stack == "job_on_error"))
    return(invisible())
  
  job_finalize(flow_name, job_name)

  flows_update_job(flow_name, job_name, list(retval = 1))
  job_log_write(flow_name, job_name, error$message, console = TRUE)
  job_log_write(flow_name, job_name, cli::ansi_strip(format(error$trace)))

  error$flow_name <- flow_name
  error$job_name <- job_name
  error_handler(cnd_entrace(error))
  invisible()
}

#' @describeIn job_start poll the process `stdout` and `stderr` streams and call job_step when ready.
#' @importFrom lubridate dminutes as.period
job_poll <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)

  job <- flows_get_job(flow_name, job_name)

  if (flows_log_get_last_run(flow_name, job_name)$status == "Forced stop") {
    job_log_write(flow_name, job_name, paste("Force stopping job, pid:", job$pid), console = TRUE)
    return(job_finalize(flow_name, job_name))
  } 
  
  timeout <- unlist(job$`timeout-minutes`) %||% 360
  elapsed <- now() - job$start_time
  if (elapsed > dminutes(timeout)) {
    job_on_error(flow_name, job_name, rlang::error_cnd(message = paste0("Job timed out after ", 
                                                                        as.period(elapsed) %>% floor, ", pid:", job$pid)))
    return(invisible())
  } 

  output <- job_read(flow_name, job_name)

  if ("process" %in% names(output) && !is.null(output[["process"]]$error)) {
    e <- job_safely_invoke(job, "run", rlang::last_error, package = T)
    job_on_error(flow_name, job_name, e)
  }

  if (job_safely_invoke(job, "get_state") %||% "finished" != "busy") {
    job_step(flow_name, job_name)
  }  
}

#' @describeIn job_start Read `stdout` and `stderr` from the process and write to log. When ready, call job_step
#' @param timeout milliseconds to wait for read before giving up, passed to `processx::process$poll_io`
#' @return character vector of output from process. Names are one or more of `output`, `error` and `process` and match the names from `processx::poll_io`
#' @importFrom rlang exprs expr
job_read <- function(flow_name, job_name, timeout = 1) {
  read_output_lines <- read_error_lines <- read <- read_all_output_lines <- read_all_error_lines <- NULL
  assert_flow_job_name(flow_name, job_name)
  job <- flows_get_job(flow_name, job_name)
  
  r_session <- job$r_session[[1]]
  
  output = NULL
  for(n in seq(100)) {
    io_state <- if(is.function(r_session$poll_io)) r_session$poll_io(timeout)
    io_names <- names(which(io_state == "ready"))
    
    if(length(io_names) == 0)
      break
    
    is_alive <- is.function(r_session$is_alive) && r_session$is_alive()
    io_funs <- exprs(process = read(),
                     output = !!ifelse(is_alive, expr(read_output_lines()), expr(read_all_output_lines())),
                     error = !!ifelse(is_alive, expr(read_error_lines()), expr(read_all_error_lines())))
    
    output <- c(output,purrr::discard(lapply(io_funs[io_names], eval, envir = r_session), ~ length(.) == 0))
    if(!is_alive)
      break
  }

  if (length(output)) {
    output_str <- lapply(output, purrr::imap, ~ paste(.y, ":", .x))
    output_str <- purrr::imap(output_str, ~ paste("[", toupper(.y), "]", .x)) %>% purrr::flatten_chr()
    job_log_write(flow_name, job_name, output_str)
  }
  
  output
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
  
  # Flush remaining output
  if(!is.null(r_session)) 
    job_read(flow_name, job_name, timeout = 1000)
  
  job_safely_invoke(job, "close")

  if (dir.exists(job$tempdir)) {
    if(unlink(job$tempdir, recursive = TRUE, force = TRUE) == 1) {
      warning(paste("Unlink of", job$tempdir, "failed"))
      return(invisible())
    }
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

#' job_safely_invoke
#'
#' @param job list as returned by [flows_get_job]
#' @param call character name of function to call
#' @param ... additional arguments passed on to `call`
#'
#' @return result of function or NULL
job_safely_invoke <- function(job, call, ...) {
  r_session <- job$r_session[[1]]
  stack <- rlang::trace_back(bottom = 2)$call %>% lapply(rlang::call_name)
  
  if (is.null(r_session) || !r_session$is_alive()) {
    
    warning(paste("Job", job$flow_name, "/", job$job_name, "has no running R session."))
    if(!any(stack == "job_finalize"))
      job_finalize(job$flow_name, job$job_name)    
    return(invisible())
  
  } else {
    r_session[[call]](...)
  } 
  
}
