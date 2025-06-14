withr::local_package("mockery")
withr::local_package("lubridate")
withr::local_package("checkmate")

local_log_dir()
withr::defer(flows_log_close())
# create the database now so that scheduled_runs are in the future
flows_log_open()
local_flows_data_table()

flow_name <- tessiflow$flows[1, flow_name]
job_name <- tessiflow$flows[1, job_name]
job_names <- tessiflow$flows[flow_name == flow_name[[1]],job_name]

job_true <- list()
job_true$scheduled_runs <- list(
  cron = c(now()),
  cron = c(now() + ddays(1))
)
job_true$`if` <- "1 == 1"
job_true$`runs-on` <- Sys.info()["nodename"]
job_true$needs <- job_names[-1]

job_false <- list()
job_false$scheduled_runs <- list(
  cron = c(now() - dyears(1)),
  cron = c(now() + ddays(1))
)
job_false$`if` <- "1 == 0"
job_false$`runs-on` <- "anothermachine"
job_false$needs <- c(job_names[3], "notajob")

# job_maybe_start ---------------------------------------------------------

test_that("job_maybe_start runs jobs when runs-on matches the current machine", {
  local_flows_data_table()
  job_start <- mock()
  stub(job_maybe_start, "job_start", job_start)
  tessiflow$flows[1, scheduled_runs := list(job_true$scheduled_runs)]

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
  tessiflow$flows[1, scheduled_runs := list(job_true$scheduled_runs)]

  tessiflow$flows[1, `if` := job_false$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `if` := job_true$`if`]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

jobs <- readRDS(test_path("jobs.Rds"))
jobs[, `:=`(
  start_time = now() - lubridate::days(2),
  end_time = now() - lubridate::ddays(1),
  retval = 0
)]
sqlite_upsert("jobs",jobs)

test_that("job_maybe_start runs jobs when needs are met and retval is 0", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)
  tessiflow$flows[1, scheduled_runs := list(job_true$scheduled_runs)]

  tessiflow$flows[1, needs := list(job_false$needs)]
  
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, needs := list(job_true$needs)]

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  flows_update_job(flow_name, job_names[2], list(start_time = jobs$start_time[[1]],end_time = NA))
  flows_update_job(flow_name, job_names[3], list(start_time = jobs$start_time[[1]],end_time = NA))

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)
  
  flows_update_job(flow_name, job_names[2], list(start_time = jobs$start_time[[1]],end_time = now()))
  
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)
  
  flows_update_job(flow_name, job_names[3], list(start_time = jobs$start_time[[1]],end_time = now()))

  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 1)
})

test_that("job_maybe_start runs jobs when needs are met and retval <> 0, but only if `if` is true", {
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)
  tessiflow$flows[1, scheduled_runs := list(job_true$scheduled_runs)]

  tessiflow$flows[1, needs := list(job_true$needs)]
  flows_update_job(flow_name, job_names[2], list(start_time = jobs$start_time[[1]],end_time = now(), retval = 1))
  
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, `if` := job_true$`if`]

  flows_update_job(flow_name, job_names[2], list(start_time = jobs$start_time[[1]],end_time = NA))
  flows_update_job(flow_name, job_names[3], list(start_time = jobs$start_time[[1]],end_time = NA))
  
  job_maybe_start(flow_name, job_name)
  expect_length(mock_args(job_start), 0)

  tessiflow$flows[1, needs := list(job_true$needs)]
  flows_update_job(flow_name, job_names[2], list(start_time = jobs$start_time[[1]],end_time = now(), retval = 1))
  flows_update_job(flow_name, job_names[3], list(start_time = jobs$start_time[[1]],end_time = now(), retval = 1))
    
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

test_that("job_maybe_start runs jobs on forced start", {
  # Only update the database, as this come from the API only
  suppressMessages(api_job_start(flow_name, job_name))
  local_flows_data_table()
  job_start <- mock(cycle = TRUE)
  stub(job_maybe_start, "job_start", job_start)

  tessiflow$flows[1, scheduled_runs := list(job_false$scheduled_runs)]

  expect_message(job_maybe_start(flow_name, job_name),paste(flow_name,"/",job_name,".+Force starting job"))
  expect_length(mock_args(job_start), 1)
  
  tessiflow$flows[1, scheduled_runs := list(job_true$scheduled_runs)]
  
  expect_message(job_maybe_start(flow_name, job_name),paste(flow_name,"/",job_name,".+Force starting job"))
  expect_length(mock_args(job_start), 2)
})

# job_make_remote_fun ----------------------------------------------------

test_that("job_make_remote_fun runs code", {
  #debugonce(job_make_remote_fun)
  expect_error(job_make_remote_fun(run_expr = "stop(\"Hello world\")")(), "Hello world")
})

test_that("job_make_remote_fun only runs if `if` true", {
  expect_error(job_make_remote_fun(NULL, "TRUE", "stop(\"Hello world\")")(), "Hello world")
  expect_message(job_make_remote_fun(NULL, "FALSE", "stop(\"Hello world\")")(), "skipping")
})

test_that("job_make_remote_fun returns NULL", {
  expect_equal(job_make_remote_fun(run_expr = "1")(),NULL)
})

test_that("job_make_remote_fun has local environment variables", {
  expect_output(job_make_remote_fun(
    list(environment = "variable"), NULL,
    "cat(Sys.getenv(\"environment\"))"
  )(), "variable")
})

test_that("job_make_remote_fun works with other shells", {
  expect_output(job_make_remote_fun(list(environment = "variable"), NULL,
    "echo $environment",
    shell = "bash -c"
  )(), "variable")
  if (.Platform$OS.type == "windows") {
    expect_output(job_make_remote_fun(list(environment = "variable"), NULL,
      "echo %environment%",
      shell = "cmd /c {0}"
    )(), "variable")
  }
})

test_that("job_make_remote_fun passes on error info for callr and other shells", {
  expect_error(job_make_remote_fun(run_expr = 'stop("I\'m an error")')(),
               "I'm an error",
               class = "rlang_error")
  expect_error(job_make_remote_fun(run_expr = "exit 1", shell = "bash -c")(),
               "exit 1.+had status 1",
               class = "rlang_error")
})


# job_start ---------------------------------------------------------------

local_flows_data_table()
stub(job_start, "job_log_write", TRUE)

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

test_that("job_start updates the flows data.table and database", {
  local_flows_data_table()
  flows_update_job <- mock(TRUE)
  stub(job_start, "flows_update_job", flows_update_job)
  job_start(flow_name, job_name)
  expect_length(mock_args(flows_update_job), 1)
  expect_names(names(unlist(mock_args(flows_update_job))[-c(1, 2)]),
    permutation.of = c("r_session", "pid", "status", "start_time", "step", "tempdir")
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

test_that("job_start gets tempdir info from the process", {
  local_flows_data_table()
  job_start(flow_name, job_name)
  expect_true(dir.exists(tessiflow$flows$tempdir[[1]]))
})

# job_step ----------------------------------------------------------------

local_flows_data_table()
job_start(flow_name, job_name)
job <- flows_get_job(flow_name, job_name)
r_session <- job$r_session[[1]]
# stub r_session$call and r_session$close
r_session <- as.environment(as.list(r_session))
r_session$.call <- r_session$call
r_session$.close <- r_session$close
r_session$call <- mock(TRUE, cycle = TRUE)
r_session$close <- mock(TRUE, cycle = TRUE)
flows_update_job(flow_name, job_name, list(r_session = list(r_session)))

stub(job_step, "job_log_write", TRUE)

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
  expect_match(deparse(mock_args(r_session$call)[[2]][[1]]), "start\\(\\)", all = FALSE)
  expect_match(deparse(mock_args(r_session$call)[[2]][[1]]), "after\\(\\)", all = FALSE)
  job_step(flow_name, job_name)
  expect_length(mock_args(r_session$call), 3)
  expect_match(deparse(mock_args(r_session$call)[[3]][[1]]), "echo.+Here", all = FALSE)
})

test_that("job_step tells callr to record debug frames if debug == T", {
  r_session$call <- r_session$.call
  withr::defer(r_session$call <- mock(TRUE, cycle = TRUE))
  
  tessiflow$flows[1,debug := T]
  withr::defer(tessiflow$flows[1,debug := F])
  
  job_step(flow_name, job_name)
  r_session$poll_io(1000)
  r_session$read()
  
  capture.output(traceback <- r_session$traceback())
  expect_gt(length(traceback),1)
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
  job_make_remote_fun <- mock(TRUE)
  stub(job_step, "job_make_remote_fun", job_make_remote_fun)
  job_step(flow_name, job_name)
  expect_length(mock_args(job_make_remote_fun), 1)
  expect_mapequal(mock_args(job_make_remote_fun)[[1]][[1]], c(job$env, job$steps[[1]]$env))
})

test_that("job_step calls job_finalize when all steps are exhausted", {
  job_finalize <- mock(TRUE)
  stub(job_step, "job_finalize", job_finalize)
  flows_update_job(flow_name, job_name, list(step = 3))

  job_step(flow_name, job_name)
  expect_length(mock_args(job_finalize), 1)
})

# job_read ----------------------------------------------------------------
rm(job_start)

local_flows_data_table()
suppressMessages(job_start(flow_name, job_name))
job <- flows_get_job(flow_name, job_name)
r_session <- job$r_session[[1]]

test_that("job_read reads from stdout and writes to the log", {
  job_log_write <- mock()
  stub(job_read, "job_log_write", job_log_write)
  r_session$call(print, list("hello world"))
  while(r_session$get_state() == "busy") {
    job_read(flow_name, job_name, timeout = 1000)
  }
  output <- unlist(purrr::map(mock_args(job_log_write),3))
  expect_match(output, "OUTPUT.+hello world", all = FALSE)
  expect_match(output, "PROCESS.+result.+hello world", all = FALSE)
})

test_that("job_read reads from stderr and writes to the log", {
  job_log_write <- mock()
  stub(job_read, "job_log_write", job_log_write)
  r_session$call(message, list("hello world"))
  while(r_session$get_state() == "busy") {
    job_read(flow_name, job_name, timeout = 1000)
  }
  output <- unlist(purrr::map(mock_args(job_log_write),3))
  expect_match(output, "ERROR.+hello world", all = FALSE)
  expect_match(output, "PROCESS.+result : $", all = FALSE)
})

test_that("job_read on finished process returns all output", {
  job_finalize <- mock()
  stub(job_poll, "job_finalize", job_finalize)
  stub(job_poll, "job_read", TRUE)
  r_session$call(eval, list(quote({
    print("hello world")
    q()
  })))
  while (r_session$is_alive()) {
    Sys.sleep(1)
  }
  expect_false(r_session$is_alive())
  expect_equal(job_read(flow_name, job_name)$output,"[1] \"hello world\"")
})

# job_poll ----------------------------------------------------------------

local_flows_data_table()
suppressMessages(job_start(flow_name, job_name))
job <- flows_get_job(flow_name, job_name)
r_session <- job$r_session[[1]]
stub(job_poll, "job_step", TRUE)

test_that("job_poll calls job_on_error on error", {
  job_on_error <- mock()
  stub(job_poll, "job_on_error", job_on_error)
  r_session$call(job_make_remote_fun(run_expr="stop('hello world')"))
  while(is.null(output <- unlist(purrr::map(mock_args(job_on_error), 3)))) {
    job_poll(flow_name, job_name)
    Sys.sleep(1)
  }
  expect_length(mock_args(job_on_error), 1)
  expect_class(mock_args(job_on_error)[[1]][[3]], "error")
  expect_match(rlang::cnd_message(mock_args(job_on_error)[[1]][[3]]), "hello world")
})

test_that("job_poll calls job_on_error on error even if it can't get last_error info",{
  job_on_error <- mock()
  stub(job_poll, "job_on_error", job_on_error)
  stub(job_poll, "rlang::last_error", NULL)
  r_session$call(stop,list('hello world'))
  
  while(is.null(output <- unlist(purrr::map(mock_args(job_on_error), 3)))) {
    job_poll(flow_name, job_name)
    Sys.sleep(1)
  }
  expect_length(mock_args(job_on_error), 1)
  expect_class(mock_args(job_on_error)[[1]][[3]], "error")
  expect_match(rlang::cnd_message(mock_args(job_on_error)[[1]][[3]]), "hello world")
})

test_that("job_poll gets rich rlang error information", {
  job_on_error <- mock()
  stub(job_poll, "job_on_error", job_on_error)
  r_session$call(job_make_remote_fun(run_expr="checkmate::assert_character(1)"))
  while(is.null(output <- unlist(purrr::map(mock_args(job_on_error), 3)))) {
    job_poll(flow_name, job_name)
    Sys.sleep(1)
  }
  expect_length(mock_args(job_on_error), 1)
  error <- mock_args(job_on_error)[[1]][[3]]
  expect_class(error, "rlang_error")
  expect_true(error$trace$call[[1]]=="checkmate::assert_character(1)")
})

test_that("job_poll calls job_step if it's ready to advance", {
  job_step <- mock()

  stub(job_poll, "job_step", job_step)
  r_session$call(print, list("hello world"))

  while(length(output <- job_read(flow_name, job_name)) == 0) {
    Sys.sleep(1)
  }
  
  job_poll(flow_name, job_name)
  expect_gte(length(mock_args(job_step)), 1)
})

test_that("job_poll calls job_on_error when timeout has passed", {
  job_on_error <- mock()
  stub(job_poll, "job_on_error", job_on_error)
  stub(job_poll, "job_read", TRUE)
  
  stub(job_poll, "now", job$start_time + dminutes(60))
  job_poll(flow_name, job_name)
  expect_length(mock_args(job_on_error), 0)
  
  stub(job_poll, "now", job$start_time + dminutes(61.12345))
  job_poll(flow_name, job_name)
  expect_length(mock_args(job_on_error), 1)
  
  flows_update_job(flow_name, job_name, list("timeout-minutes" = list(NULL)))
  
  stub(job_poll, "now", job$start_time + dminutes(360))
  job_poll(flow_name, job_name)
  expect_length(mock_args(job_on_error), 1)

  stub(job_poll, "now", job$start_time + dminutes(361.12345))
  job_poll(flow_name, job_name)
  expect_length(mock_args(job_on_error), 2)
  
  expect_class(mock_args(job_on_error)[[1]][[3]], "error")
  expect_match(mock_args(job_on_error)[[1]][[3]]$message, "1H 1M 7S")
  expect_class(mock_args(job_on_error)[[2]][[3]], "error")
  expect_match(mock_args(job_on_error)[[2]][[3]]$message, "6H 1M 7S")
  tessiflow$flows[get("flow_name") == flow_name & get("job_name") == job_name, status:="Running"]
  
})

rm(job_poll)
test_that("job_poll calls job_finalize when a job has finished on its own", {
  # This is a bit of an integration test, checking that finalize is eventually called...
  r_session$run(q)
  suppressWarnings(suppressMessages(
    expect_message(job_poll(flow_name, job_name), "Finalizing job")
  ))
})

test_that("job_poll calls job_finalize on forced stop", {
  # Only update the database, as this comes from the API only
  api_job_stop(flow_name, job_name)
  tessiflow$flows[get("flow_name") == flow_name & get("job_name") == job_name, status:="Running"]
  
  job_finalize <- mock()
  job_read <- mock()
  stub(job_poll, "job_finalize", job_finalize)
  stub(job_poll, "job_read", job_read)

  expect_message(job_poll(flow_name, job_name),paste(flow_name,"/",job_name,".+Force stopping job, pid:"))
  expect_length(mock_args(job_finalize), 1)
  expect_length(mock_args(job_read), 0)
})



# job_on_error ------------------------------------------------------------

local_flows_data_table()
suppressMessages(job_start(flow_name, job_name))
job <- flows_get_job(flow_name, job_name)
r_session <- job$r_session[[1]]
# stub r_session$close so that job_finalize doesn't really kill it
r_session <- as.environment(as.list(r_session))
r_session$.close <- r_session$close
r_session$close <- mock(TRUE, cycle = TRUE)
flows_update_job(flow_name, job_name, list(r_session = list(r_session)))

stub(job_on_error, "job_log_write", TRUE)
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

test_that("job_on_error won't infinitely regress on internal errors", {
  error_handler <- mock()
  stub(job_on_error, "job_finalize", function(...){job_on_error(flow_name,job_name,
                          rlang::error_cnd(message = "I'm an internal error"))})
  stub(job_on_error, "error_handler", error_handler)
  job_on_error(flow_name, job_name, rlang::error_cnd(message = "test error", trace = rlang::trace_back()))
  expect_length(mock_args(error_handler), 1)
  expect_equal(rlang::cnd_message(mock_args(error_handler)[[1]][[1]]),"test error")
})

# job_finalize ------------------------------------------------------------

local_flows_data_table()
flows_update_job(flow_name, job_name, list(r_session = list(r_session), tempdir=r_session$run(tempdir)))
flows_update_job <- mock(TRUE, cycle = TRUE)
stub(job_finalize, "flows_update_job", flows_update_job)
stub(job_finalize, "job_log_write", TRUE)
stub(job_finalize, "unlink", 0)

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

test_that("job_finalize complains if tessiflow.debug is not set but job$debug is TRUE and job$retval has error", {
  tessiflow$flows[1,`:=`(debug = T, retval = 1)]
  withr::defer(tessiflow$flows[1,`:=`(debug = F, retval = 0)])
  
  job_log_write <- mock(TRUE, cycle = TRUE)
  stub(job_finalize, "job_log_write", job_log_write)
  expect_warning(job_finalize(flow_name, job_name), "Job 1 Errored")
  expect_length(mock_args(job_log_write), 2)
  expect_match(mock_args(job_log_write)[[1]][[3]], "tessiflow\\.debug is not set")
  expect_equal(mock_args(job_log_write)[[1]][["console"]], TRUE)
})

test_that("job_finalize calls preserve_debug_frames if job$debug is TRUE", {
  tessiflow$flows[1,`:=`(debug = T, retval = 1, pid = 12345)]
  withr::defer(tessiflow$flows[1,`:=`(debug = F, retval = 0)])

  stub(job_finalize,"preserve_debug_frames",function(...) {filename <<- list(...)})
  stub(job_finalize, "config::get", "tessiflow.debug")

  expect_warning(job_finalize(flow_name, job_name), "Job 1 Errored")
  expect_equal(r_session$run(function(){filename}), list("tessiflow.debug/Dummy workflow_Job 1_12345.debug"))
  
})


# unstub close
r_session$close <- r_session$.close
test_that("job_finalize closes the session and all subprocesses", {
  # unstub file.remove
  stub(job_finalize,"unlink",base::unlink)
  expect_equal(job$r_session[[1]]$get_state(), "idle")
  job$r_session[[1]]$run(callr::r_bg, list(Sys.sleep, list(10)), package = TRUE)
  children <- ps::ps_children(job$r_session[[1]]$as_ps_handle())
  job_finalize(flow_name, job_name)
  expect_equal(job$r_session[[1]]$get_state(), "finished")
  expect_equal(sapply(children, ps::ps_is_running), rep(FALSE, length(children)))
})

test_that("job_finalize warns if there's no session to close", {
  expect_warning(job_finalize(flow_name, job_name), "no running R session")
})

# unstub unlink
rm(job_finalize)

test_that("job_finalize warns when it fails to cleanup the tempdir", {
  suppressMessages(job_start(flow_name, job_name))
  job_on_error <- mock()
  stub(job_finalize,"dir.exists",TRUE)
  stub(job_finalize,"unlink",1)
  stub(job_finalize,"job_on_error",job_on_error)
  
  expect_warning(job_finalize(flow_name, job_name),"Unlink.+failed")
  expect_equal(flows_get_job(flow_name, job_name)$status,"Running")

})

  
test_that("job_finalize cleans up the tempdir", {
  suppressMessages(job_start(flow_name, job_name))
  
  tessiflow$flows$r_session[[1]]$kill()
  while(tessiflow$flows$r_session[[1]]$is_alive()) 
    Sys.sleep(1)
  expect_true(any(dir.exists(tessiflow$flows$tempdir)))
  suppressMessages(expect_warning(job_finalize(flow_name, job_name),"no running R session"))

  expect_false(any(dir.exists(tessiflow$flows$tempdir)))
})


test_that("job_finalize reads all remaining output from the process if it has died", {
  # Only relevant in Windows because *nix doesn't let a process die while its stdout remains unread
  if(Sys.info()["sysname"] == "Windows"){
    local_flows_data_table()
    flow_name <- "Dummy workflow 2"
    job_name <- "Job 3"
    suppressMessages(job_start(flow_name, job_name))
    job <- flows_get_job(flow_name, job_name)
    r_session <- job$r_session[[1]]
    r_session$call(function() {
      print(as.list(seq(1000)))
      q()
    })
    
    job_log_write <- mock()
    stub(job_read,"job_log_write",job_log_write)
    stub(job_finalize,"job_read",job_read)
    
    while(r_session$is_alive())
      Sys.sleep(1)
    
    suppressMessages(expect_warning(job_finalize(flow_name, job_name),"no running R session"))
    expect_length(mock_args(job_log_write),1)
    output <- unlist(mock_args(job_log_write))
    expect_match(output,"\\[1\\] 1$", all = FALSE)
    expect_match(output,"\\[1\\] 1000$", all = FALSE)
  }
})

test_that("job_finalize reads all remaining output from the process if it is still running", {
  local_flows_data_table()
  flow_name <- "Dummy workflow 2"
  job_name <- "Job 3"
  suppressMessages(job_start(flow_name, job_name))
  job <- flows_get_job(flow_name, job_name)
  r_session <- job$r_session[[1]]
  r_session$call(function() {
    print(as.list(seq(1000)))
  })
  
  while(r_session$get_state() != "busy")
    Sys.sleep(1)
  
  job_log_write <- mock()
  stub(job_read,"job_log_write",job_log_write)
  stub(job_finalize,"job_read",job_read)
  
  suppressMessages(job_finalize(flow_name, job_name))
  expect_length(mock_args(job_log_write),1)
  output_length <- nchar(unlist(mock_args(job_log_write)))
  output <- unlist(mock_args(job_log_write))[output_length == max(output_length)][[1]]
  # output format is "result : 27", 
  expect_match(output,"result : 1")
  expect_match(output,"result : 1000")
})

# job_reset ------------------------------------------------------------

local_flows_data_table()
job_log_write <- mock(TRUE, cycle = TRUE)
stub(job_reset, "job_log_write", job_log_write)

test_that("job_reset updates the flows data.table", {
  old_flows <- copy(tessiflow$flows)
  old_flows[, `:=`(
    step = NA_integer_, pid = NA_integer_, r_session = list(NULL),
    scheduled_runs = lapply(`on.schedule`, lapply, parse_cron)
  )]
  tessiflow$flows[, `:=`(
    start_time = now(),
    step = 1,
    status = "Running",
    pid = 123,
    r_session = list("process")
  )]
  tessiflow$flows[, Vectorize(job_reset)(flow_name, job_name)]
  expect_equal(tessiflow$flows, old_flows)
})

test_that("job_finalize writes to the log file and console", {
  job_reset(flow_name, job_name)
  expect_length(mock_args(job_log_write), 7)
  expect_match(mock_args(job_log_write)[[7]][[3]], "Resetting job")
  expect_equal(unlist(mock_args(job_log_write)[[7]])[4], c(console = "TRUE"))
})

