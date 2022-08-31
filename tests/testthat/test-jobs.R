withr::local_package("mockery")
withr::local_package("lubridate")
withr::local_package("checkmate")

local_log_dir()
local_flows_data_table()

flow_name <- tessiflow$flows[1, flow_name]
job_name <- tessiflow$flows[1, job_name]

job_true <- list()
job_true$scheduled_runs <- list(
  cron = c(now()),
  cron = c(now() + ddays(1))
)
job_true$`if` <- "1 == 1"
job_true$`runs-on` <- Sys.info()["nodename"]
job_true$needs <- list("Job 2")

job_false <- list()
job_false$scheduled_runs <- list(
  cron = c(now() - dyears(1)),
  cron = c(now() + ddays(1))
)
job_false$`if` <- "1 == 0"
job_false$`runs-on` <- "anothermachine"
job_false$needs <- list("Job 3", "notajob")

test_that("job_maybe_start runs jobs when runs-on matches the current machine", {
  local_flows_data_table()
  job_start <- mock()
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, `runs-on` := job_false$`runs-on`]
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `runs-on` := job_true$`runs-on`]
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

test_that("job_maybe_start runs jobs when if is true", {
  local_flows_data_table()
  job_start <- mock()
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, `if` := job_false$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `if` := job_true$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

jobs <- readRDS(test_path("jobs.Rds"))
jobs[, `:=`(
  end_time = now() - lubridate::ddays(1),
  retval = 0
)]
last_run_times <- function(flow, job) {
  jobs[flow_name == flow & job_name == job, ]
}
stub(job_maybe_start, "flows_log_get_last_run", last_run_times)

test_that("job_maybe_start runs jobs when needs are met and retval is 0", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, needs := list(job_false$needs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, needs := list(job_true$needs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  jobs[job_name == "Job 2", `:=`(end_time = now())]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

test_that("job_maybe_start runs jobs when needs are met and retval <> 0, but only if `if` is true", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, needs := list(job_true$needs)]
  jobs[job_name == "Job 2", `:=`(end_time = now(), retval = 1)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `if` := job_true$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

test_that("job_maybe_start runs jobs when they are scheduled", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, scheduled_runs := list(job_false$scheduled_runs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, scheduled_runs := list(job_true$scheduled_runs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})


# job_make_remote_expr ----------------------------------------------------

test_that("job_make_remote_expr runs code", {
  expect_error(eval(job_make_remote_expr(run_expr = "stop(\"Hello world\")")), "Hello world")
})

test_that("job_make_remote_expr only runs if `if` true", {
  expect_error(eval(job_make_remote_expr(NULL, "TRUE", "stop(\"Hello world\")")), "Hello world")
  expect_message(eval(job_make_remote_expr(NULL, "FALSE", "stop(\"Hello world\")")), "skipping")
})

test_that("job_make_remote_expr has local environment variables", {
  expect_equal(eval(job_make_remote_expr(
    list(environment = "variable"), NULL,
    "Sys.getenv(\"environment\")"
  )), "variable")
})

test_that("job_make_remote_expr works with other shells", {
  expect_equal(eval(job_make_remote_expr(list(environment = "variable"), NULL,
    "echo $environment",
    shell = "bash -c"
  )), "variable")
  if (.Platform$OS.type == "windows") {
    expect_equal(eval(job_make_remote_expr(list(environment = "variable"), NULL,
      "echo %environment%",
      shell = "cmd /c {0}"
    )), "variable")
  }
})


# job_start ---------------------------------------------------------------

local_flows_data_table()
flow_name <- "Dummy workflow"
job_name <- "Job 1"

suppressMessages({
  test_that("job_start spins up an r session", {
    job_start(flow_name, job_name)
    job <- flows_get_job(flow_name, job_name)
    expect_class(job$r_session[[1]], "r_session")
    expect_equal(job$r_session[[1]]$get_state(), "idle")
  })

  test_that("job_start spins up an r session but only if one doesn't already exist", {
    job <- flows_get_job(flow_name, job_name)
    expect_warning(job_start(flow_name, job_name), "already been started")
    job2 <- flows_get_job(flow_name, job_name)
    expect_equal(job2$pid, job$pid)
  })

  # stub r_session$new
  stub(job_start, "r_session$new", flows_get_job(flow_name, job_name)$r_session[[1]])
  job <- flows_get_job(flow_name, job_name)
  r_session <- job$r_session[[1]]

  test_that("job_start updates the flows data.table and database", {
    local_flows_data_table()
    flows_update_job <- mock(TRUE)
    stub(job_start, "flows_update_job", flows_update_job)
    job_start(flow_name, job_name)
    expect_length(mock_args(flows_update_job), 1)
    expect_names(names(unlist(mock_args(flows_update_job))[-c(1, 2)]),
      permutation.of = c("r_session", "pid", "status", "start_time", "step")
    )
  })

  test_that("job_start writes to the log file and console", {
    local_flows_data_table()
    job_log_write <- mock(TRUE)
    stub(job_start, "job_log_write", job_log_write)
    job_start(flow_name, job_name)
    expect_length(mock_args(job_log_write), 1)
    expect_match(mock_args(job_log_write)[[1]][[3]], "Starting job")
    expect_equal(unlist(mock_args(job_log_write)[[1]])[4], c(console = "TRUE"))
  })


  # job_step ----------------------------------------------------------------

  local_flows_data_table()
  job_start(flow_name, job_name)
  # stub r_session$call and r_session$close
  r_session <- as.environment(as.list(r_session))
  r_session$.call <- r_session$call
  r_session$.close <- r_session$close
  r_session$call <- mock(TRUE, cycle = TRUE)
  r_session$close <- mock(TRUE, cycle = TRUE)
  flows_update_job(flow_name, job_name, list(r_session = list(r_session)))

  test_that("job_step updates the flows data.table and database", {
    flows_update_job <- mock(TRUE)
    stub(job_step, "flows_update_job", flows_update_job)
    job_step(flow_name, job_name)
    expect_length(mock_args(flows_update_job), 1)
    expect_names(lapply(mock_args(flows_update_job)[[1]][-c(1, 2)], names)[[1]],
      permutation.of = c("step")
    )
  })

  test_that("job_step calls the next step", {
    job_step(flow_name, job_name)
    expect_length(mock_args(r_session$call), 2)
    expect_match(deparse(mock_args(r_session$call)[[2]][[2]][[1]]), "start.+after", all = FALSE)
    job_step(flow_name, job_name)
    expect_length(mock_args(r_session$call), 3)
    expect_match(deparse(mock_args(r_session$call)[[3]][[2]][[1]]), "echo.+Here", all = FALSE)
  })

  flows_update_job(flow_name, job_name, list(step = 0))
  test_that("job_step writes to the log file and console", {
    job_log_write <- mock(TRUE)
    stub(job_step, "job_log_write", job_log_write)
    job_step(flow_name, job_name)
    expect_length(mock_args(job_log_write), 1)
    expect_match(mock_args(job_log_write)[[1]][[3]], "Beginning step 1")
    expect_equal(unlist(mock_args(job_log_write)[[1]])[4], c(console = "TRUE"))
  })

  flows_update_job(flow_name, job_name, list(step = 0))
  test_that("job_step passed on the flow and step environment variables", {
    job_make_remote_expr <- mock(TRUE)
    stub(job_step, "job_make_remote_expr", job_make_remote_expr)
    job <- flows_get_job(flow_name, job_name)
    job_step(flow_name, job_name)
    expect_length(mock_args(job_make_remote_expr), 1)
    expect_mapequal(mock_args(job_make_remote_expr)[[1]][[1]], c(job$env, job$steps[[1]]$env))
  })

  test_that("job_step calls job_finalize when all steps are exhausted", {
    job_finalize <- mock(TRUE)
    stub(job_step, "job_finalize", job_finalize)
    flows_update_job(flow_name, job_name, list(step = 2))

    job_step(flow_name, job_name)
    expect_length(mock_args(job_finalize), 1)
  })

  # job_poll ----------------------------------------------------------------

  stub(job_poll, "job_step", TRUE)

  test_that("job_poll reads from stdout and writes to the log", {
    job_log_write <- mock()
    stub(job_poll, "job_log_write", job_log_write)
    r_session$.call(print, list("hello world"))
    Sys.sleep(1)
    job_poll(flow_name, job_name)
    output <- unlist(purrr::map(mock_args(job_log_write), 3))
    expect_match(output, "OUTPUT.+hello world", all = FALSE)
    expect_match(output, "PROCESS.+result.+hello world", all = FALSE)
  })

  test_that("job_poll reads from stderr and writes to the log", {
    job_log_write <- mock()
    stub(job_poll, "job_log_write", job_log_write)
    r_session$.call(message, list("hello world"))
    Sys.sleep(1)
    job_poll(flow_name, job_name)
    output <- unlist(purrr::map(mock_args(job_log_write), 3))
    expect_match(output, "ERROR.+hello world", all = FALSE)
    expect_match(output, "PROCESS.+result : $", all = FALSE)
  })

  test_that("job_poll calls job_on_error on error", {
    job_on_error <- mock()
    stub(job_poll, "job_on_error", job_on_error)
    r_session$.call(stop, list("hello world"))
    Sys.sleep(1)
    job_poll(flow_name, job_name)
    expect_length(mock_args(job_on_error), 1)
    expect_class(mock_args(job_on_error)[[1]][[3]], "error")
  })

  test_that("job_poll calls job_step if it's ready to advance", {
    job_step <- mock()
    stub(job_poll, "job_step", job_step)
    r_session$.call(print, list("hello world"))
    Sys.sleep(1)
    job_poll(flow_name, job_name)
    expect_length(mock_args(job_step), 1)
  })

  # job_on_error ------------------------------------------------------------

  stub(job_on_error, "job_finalize", TRUE)
  test_that("job_on_error updates the database and data.table", {
    flows_update_job <- mock()
    stub(job_on_error, "flows_update_job", flows_update_job)
    stub(job_on_error, "error_handler", NULL)
    job_on_error(flow_name, job_name, rlang::error_cnd(message = "test error"))
    expect_length(mock_args(flows_update_job), 1)
    expect_equal(mock_args(flows_update_job)[[1]][[3]], list(retval = 1))
  })

  test_that("job_on_error writes to the log file and console", {
    job_log_write <- mock()
    stub(job_on_error, "job_log_write", job_log_write)
    stub(job_on_error, "error_handler", NULL)
    job_on_error(flow_name, job_name, rlang::error_cnd(message = "test error", trace = rlang::trace_back()))
    expect_length(mock_args(job_log_write), 2)
    expect_equal(mock_args(job_log_write)[[1]][[3]], "test error")
    expect_equal(mock_args(job_log_write)[[1]][["console"]], TRUE)
    expect_equal(mock_args(job_log_write)[[2]][[3]], "x")
  })

  test_that("job_on_error calls job_finalize", {
    job_finalize <- mock()
    stub(job_on_error, "job_finalize", job_finalize)
    stub(job_on_error, "error_handler", NULL)
    job_on_error(flow_name, job_name, rlang::error_cnd(message = "test error", trace = rlang::trace_back()))
    expect_length(mock_args(job_finalize), 1)
  })

  test_that("job_on_error calls error_handler with flow and job info", {
    error_handler <- mock()
    stub(job_on_error, "error_handler", error_handler)
    job_on_error(flow_name, job_name, rlang::error_cnd(message = "test error", trace = rlang::trace_back()))
    expect_length(mock_args(error_handler), 1)
    expect_equal(mock_args(error_handler)[[1]][[1]]$flow_name, flow_name)
    expect_equal(mock_args(error_handler)[[1]][[1]]$job_name, job_name)
  })

  # job_finalize ------------------------------------------------------------

  local_flows_data_table()
  flows_update_job(flow_name, job_name, list(r_session = list(r_session)))
  flows_update_job <- mock(TRUE, cycle = TRUE)
  stub(job_finalize, "flows_update_job", flows_update_job)

  test_that("job_finalize updates the flows data.table and database", {
    job_finalize(flow_name, job_name)
    expect_length(mock_args(flows_update_job), 1)
    expect_names(lapply(mock_args(flows_update_job)[[1]][-c(1, 2)], names)[[1]],
      permutation.of = c("r_session", "status", "end_time", "retval")
    )
  })

  test_that("job_finalize writes to the log file and console", {
    job_log_write <- mock(TRUE)
    stub(job_finalize, "job_log_write", job_log_write)
    job_finalize(flow_name, job_name)
    expect_length(mock_args(job_log_write), 1)
    expect_match(mock_args(job_log_write)[[1]][[3]], "Finalizing job")
    expect_equal(unlist(mock_args(job_log_write)[[1]])[4], c(console = "TRUE"))
  })

  test_that("job_finalize closes the session", {
    expect_equal(job$r_session[[1]]$get_state(), "idle")
    job_finalize(flow_name, job_name)
    r_session$.close()
    expect_equal(job$r_session[[1]]$get_state(), "finished")
  })

  test_that("job_finalize warns if there's no session to close", {
    local_flows_data_table()
    expect_warning(job_finalize(flow_name, job_name), "no running R session")
  })
})


# job_reset ------------------------------------------------------------

local_flows_data_table()
job_log_write <- mock(TRUE, cycle = TRUE)
stub(job_reset, "job_log_write", job_log_write)

test_that("job_reset updates the flows data.table", {
  old_flows <- copy(tessiflow$flows)
  old_flows[, `:=`(
    step = NA_integer_, pid = NA_integer_, r_session = list(NULL),
    scheduled_runs = lapply(`on.schedule`[[1]], lapply, parse_cron)
  )]
  tessiflow$flows[1:2, `:=`(
    start_time = now(),
    step = 1,
    status = "Running",
    pid = 123,
    r_session = list("process")
  )]
  job_reset(flow_name, job_name)
  expect_equal(tessiflow$flows[1, ], old_flows[1, ])
  expect_equal(tessiflow$flows[2, status], "Running")
})

test_that("job_finalize writes to the log file and console", {
  job_reset(flow_name, job_name)
  expect_length(mock_args(job_log_write), 2)
  expect_match(mock_args(job_log_write)[[2]][[3]], "Resetting job")
  expect_equal(unlist(mock_args(job_log_write)[[2]])[4], c(console = "TRUE"))
})

# job_stop ----------------------------------------------------------------

local_flows_data_table()
job_log_write <- mock(TRUE, cycle = TRUE)
stub(job_stop, "job_log_write", job_log_write)
stub(job_stop, "job_finalize", TRUE)

test_that("job_stop updates the flows data.table and database", {
  job_stop(flow_name, job_name)
  expect_equal(flows_get_job(flow_name, job_name)$status, "Stopped")
})

test_that("job_stop writes to the log file and console", {
  job_stop(flow_name, job_name)
  expect_length(mock_args(job_log_write), 2)
  expect_match(mock_args(job_log_write)[[2]][[3]], "Stopping job")
  expect_equal(unlist(mock_args(job_log_write)[[2]])[4], c(console = "TRUE"))
})
