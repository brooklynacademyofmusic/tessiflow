
#' job_maybe_start
#'
#' check to see if it's time to start a job and call the job runner if it is.
#' the job will be started if following conditions are all true:
#' - `runs-on` must match the computer name as listed in `Sys.info()["nodename"]`
#' - `if` must be true in the current context
#' - `needs` must have finished running since the last time this ran (with retval = 0 unless `if` evaluates to true)
#' - the most recent run in `scheduled_runs` is after the last actual run time for this job
#'
#' @param flow_name string workflow name
#' @param job_name string job name
#'
#' @return invisibly
#' @importFrom utils tail
#'
job_maybe_start <- function(flow_name, job_name) {
  job <- flows_get_job(flow_name, job_name)

  last_run <- flows_log_get_last_run(job$flow_name, job$job_name)
  if (nrow(last_run) == 0) {
    last_run <- rbind(last_run, list(end_time = as.double(flows_log_get_create_time())))
  }

  # check runs-on
  if (!is.null(job_runs_on <- job$`runs-on`)) {
    check_runs_on <- job_runs_on == Sys.info()["nodename"]
  }

  # check if
  if (!is.null(job_if <- job$`if`)) {
    check_if <- eval(rlang::parse_expr(as.character(job_if)))
  }

  # check needs
  if (length(job_needs <- job$needs[[1]]) > 0) {
    dependencies <- flows_log_get_last_run(job$flow_name, job_needs)
    check_needs <- all(dependencies$end_time > last_run$end_time) &&
      (all(dependencies$retval == 0) || exists("check_if") && check_if) &&
      nrow(dependencies) == length(job_needs)
  }

  # check schedule
  if (length(job_scheduled_runs <- job$scheduled_runs[[1]]) > 0) {
    check_schedule <- any(job_scheduled_runs %>% lapply(purrr::keep, ~ . < now()) %>% lapply(tail, 1) >
      last_run$end_time)
  }

  if ((!exists("check_runs_on") || check_runs_on) &&
    (!exists("check_if") || check_if) &&
    (!exists("check_needs") || check_needs) &&
    (!exists("check_schedule") || !is.na(check_schedule) && check_schedule)) {
    job_start(flow_name, job_name)
  }
}

#' job_start
#'
#' Run the job in a callr process
#'
#' @param flow_name string workflow name
#' @param job_name string job name
#'
#' @importFrom callr r_session r_session_options
#'
#' @return invisibly
#'
job_start <- function(flow_name, job_name) {
  # spin up a callr process
  job <- flows_get_job(flow_name, job_name)

  if (is.null(job$r_session) || job$r_session[[1]]$get_state() == "finished") {
    r_session <- r_session$new(options = r_session_options(
      stdout = "|",
      stderr = "|",
      env = unlist(job$env)
    ), wait = TRUE)
    tessiflow$flows[
      flow_name == job$flow_name & job_name == job$job_name,
      `:=`(
        r_session = list(r_session),
        pid = r_session$get_pid(),
        step = 0,
        start_time = now(),
        status = "Running"
      )
    ]

    job_log_write(flow_name, job_name, paste("Starting job, pid:", r_session$get_pid()))
    job_step(flow_name, job_name)
  } else {
    warning(paste("Job", job$flow_name, "/", job$job_name, "has already been started!"))
  }

  return(invisible())
}

#' job_step
#'
#' Run the next step in a job
#'
#' @param flow_name string workflow name
#' @param job_name string job name
#'
#' @return invisibly
#'
job_step <- function(flow_name, job_name) {
  job <- flows_get_job(flow_name, job_name)

  if (is.null(job$r_session) || job$r_session[[1]]$get_state() == "finished") {
    warning(paste("Job", flow_name, "/", job_name, "has no running R session."))
    return(invisible())
  }

  if (job$step == length(job$steps)) {
    return(job_finalize(flow_name, job_name))
  }

  current_step <- job$steps[[job$step + 1]]
  tessiflow$flows[
    flow_name == job$flow_name & job_name == job$job_name,
    `:=`(step = job$step + 1)
  ]

  job_log_write(flow_name, job_name, paste("Beginning step", job$step, ":", current_step$name))

  rlang::expr(withr::with_envvar(!!current_step$env, {
    Sys.getenv()
    if (! !!rlang::parse_expr(as.character(current_step$`if`))) {
      message("'if' is not true, skipping")
      invisible(NULL)
    } else {
      !!as.call(c(`{`, rlang::parse_exprs(as.character(current_step$run))))
    }
  })) -> remote_expr

  job$r_session[[1]]$call(eval, list(remote_expr))

  return(invisible())
}


job_poll <- function(flow_name, job_name) {
  job <- flows_get_job(flow_name, job_name)

  if (is.null(job$r_session) || job$r_session[[1]]$get_state() == "finished") {
    warning(paste("Job", flow_name, "/", job_name, "has no running R session."))
    return(invisible())
  }

  errored <- FALSE

  while (TRUE) {
    io_state <- job$r_session[[1]]$poll_io(1)
    if (!"ready" %in% io_state) {
      break
    }
    io_names <- names(which(io_state == "ready"))
    if ("error" %in% io_names) {
      errored <- TRUE
    }
    io_funs <- rlang::parse_exprs(
      gsub("_process_lines", "", paste("read", io_names, "lines()", sep = "_"))
    )
    names(io_funs) <- io_names
    output <- purrr::discard(sapply(io_funs, eval, envir = job$r_session[[1]]), ~ length(.) == 0)
    if (length(output)) {
      output <- purrr::map2(
        output, names(output),
        ~ paste(
          "[", toupper(.y), "]",
          if (!is.character(.x)) {
            capture.output(dput(.x))
          } else {
            .x
          }
        )
      )
      job_log_write(flow_name, job_name, purrr::flatten_chr(output))
    }
  }

  if (errored) {
    tessiflow$flows[
      flow_name == job$flow_name & job_name == job$job_name,
      retval := 1
    ]
    job_finalize(flow_name, job_name)
  }

  if (job$r_session[[1]]$get_state() == "idle") {
    job_step(flow_name, job_name)
  }
}

job_finalize <- function(flow_name, job_name) {
  job <- flows_get_job(flow_name, job_name)

  if (is.null(job$r_session) || job$r_session[[1]]$get_state() == "finished") {
    warning(paste("Job", flow_name, "/", job_name, "has no running R session."))
    return(invisible())
  }

  if (!is.na(job$retval)) {
    warning("Errored")
  } else {
    tessiflow$flows[
      flow_name == job$flow_name & job_name == job$job_name,
      `:=`(
        r_session = list(),
        pid = NA_integer_,
        retval = 0,
        end_time = now(),
        status = "Finished"
      )
    ]
  }

  job$r_session[[1]]$close()
}
