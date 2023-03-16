withr::local_package("mockery")
withr::local_package("devtools")

# tessiflow_pid_lock/unlock -----------------------------------------------

test_that("tessiflow_pid_lock creates a pid file", {
  tessiflow_pid_lock(tempdir())
  expect_equal(readLines(file.path(tempdir(), "tessiflow.pid")), as.character(Sys.getpid()))
})
test_that("tessiflow_pid_lock refuses to create a pid file if one already exists", {
  expect_error(tessiflow_pid_lock(tempdir()), "Found running tessiflow")
})
test_that("tessiflow_pid_unlock refuses to delete a mismatched pid file", {
  stub(tessiflow_pid_unlock, "Sys.getpid", -1)
  expect_error(tessiflow_pid_unlock(tempdir()), "pid does not match")
  expect_true(file.exists(file.path(tempdir(), "tessiflow.pid")))
})
test_that("tessiflow_pid_unlock deletes a matching pid file", {
  tessiflow_pid_unlock(tempdir())
  expect_false(file.exists(file.path(tempdir(), "tessiflow.pid")))
})

# tessiflow_run ---------------------------------------------------------

run_fun <- function() {}
body(run_fun) <-
  rlang::expr({
    local_log_dir(envir = new.env())
    mockery::stub(tessiflow_run, "flows_main", function() {
      Sys.sleep(10) # has to be long enough to allow the process to persist between tests
    })
    mockery::stub(tessiflow_run, "performance_main", TRUE)
    mockery::stub(tessiflow_run, "api_start", TRUE)
    
    mockery::stub(tessiflow_run, "callr::r_bg", callr::r)
    mockery::stub(tessiflow_run, "config::get", !!config::get("tessiflow.log"))
    tessiflow_run()
  })


consume_output_lines <- function(process) {
  # consume the rest of the output lines
  while (length(output <- process$read_output_lines()) == 0 && process$is_alive()) {
    process$poll_io(10000)
  }
  while (length(current_output <- process$read_output_lines()) > 0) {
    output <- append(output, current_output)
    Sys.sleep(1)
  }
  output
}

test_that("tessiflow_run refuses to start if tessiflow is already running", {
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")), 0)
  local_log_dir()

  p1 <- callr::r_bg(run_fun, package = "tessiflow", stderr = "2>&1")
  p1_output <- consume_output_lines(p1)
  expect_match(p1_output, "Starting tessiflow")
  expect_gte(length(ps::ps_find_tree("tessiflow-daemon")), 1)

  expect_error(tessiflow_run(), "Found running tessiflow")
  expect_gte(length(ps::ps_find_tree("tessiflow-daemon")), 1)

  p1$kill_tree()
})

test_that("tessiflow_run starts all sub-processes and logs to a log file", {
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")), 0)
  local_log_dir()

  stub(tessiflow_run, "flows_main", function() {
    message("Running flows_main()")
    get("api",envir=parent.frame())$wait()
    get("performance",envir=parent.frame())$wait()
  })
  stub(tessiflow_run, "performance_main", function() {
    message("Running performance_main()")
  })
  stub(tessiflow_run, "api_start", function() {
    message("Running api_start()")
  })

  suppressMessages(expect_message(tessiflow_run(), "Running flows_main", all = FALSE))

  logdata <- readLines(file.path(config::get("tessiflow.log"), "tessiflow-daemon.log"))
  expect_match(logdata, "Starting tessiflow", all = FALSE)
  expect_match(logdata, "Running flows_main", all = FALSE)
  expect_match(logdata, "Running performance_main", all = FALSE)
  expect_match(logdata, "Running api_start", all = FALSE)
  expect_equal(length(logdata), 4)
})


# tessiflow_start ---------------------------------------------------------

test_that("tessiflow_start sets the working directory", {
  stub(tessiflow_start, "tessiflow_run", getwd)
  stub(tessiflow_start, "Sys.getenv", tempdir())
  expect_silent(tessiflow_start())
  expect_equal(tessiflow_start(), tools::file_path_as_absolute(tempdir()))
})

test_that("tessiflow_start pauses to show errors to humans", {
  countdown <- mock(TRUE)
  stub(tessiflow_start, "countdown", countdown)
  stub(tessiflow_start, "tessiflow_run", function() stop("I am a really bad error"))
  stub(tessiflow_start, "Sys.getenv", tempdir())
  expect_output(tessiflow_start(), "I am a really bad error")
  expect_equal(mock_args(countdown)[[1]][[1]], 30)
})

# tessiflow_stop ----------------------------------------------------------

test_that("tessiflow_stop kills all running jobs", {
  local_log_dir()
  p1 <- callr::r_bg(run_fun, package = "tessiflow", stderr = "2>&1")
  consume_output_lines(p1)

  expect_gte(length(ps::ps_find_tree("tessiflow-daemon")), 1)

  tessiflow_stop()
  Sys.sleep(1)

  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")), 0)
})

# tessiflow_enable --------------------------------------------------------

test_that("tessiflow_enable schedules tessiflow", {
  schedule_schtasks <- mock()
  schedule_crontab <- mock()
  Platform <- .Platform
  stub(tessiflow_enable, "schedule_schtasks", schedule_schtasks)
  stub(tessiflow_enable, "schedule_crontab", schedule_crontab)

  Platform$OS.type <- "windows"
  assign(".Platform", Platform, envir = environment(tessiflow_enable))
  tessiflow_enable()

  Platform$OS.type <- "linux"
  assign(".Platform", Platform, envir = environment(tessiflow_enable))
  tessiflow_enable()

  expect_match(as.character(mock_args(schedule_schtasks)[[1]][[1]]), "tessiflow::tessiflow_start\\(\\)", all = FALSE)
  expect_match(as.character(mock_args(schedule_crontab)[[1]][[1]]), "tessiflow::tessiflow_start\\(\\)", all = FALSE)
})

test_that("tessiflow_enable schedules a runnable script", {
  local_log_dir()
  schedule_schtasks <- mock()
  Platform <- .Platform
  stub(tessiflow_enable, "schedule_schtasks", schedule_schtasks)

  Platform$OS.type <- "windows"
  assign(".Platform", Platform, envir = environment(tessiflow_enable))
  tessiflow_enable()

  local_dir(rprojroot::find_testthat_root_file())

  # create a config.yml file to mimic what happens in a normal install
  withr::local_file(list("config.yml" = yaml::write_yaml(
    list(default = list(
      tessiflow.log = config::get("tessiflow.log"),
      tessiflow.d = config::get("tessiflow.d"),
      tessiflow.port = 0
    )),
    "config.yml"
  )))

  p <- callr::r_bg(system, list(mock_args(schedule_schtasks)[[1]][[1]]),
    env = c(
      R_CONFIG_FILE = "config.yml",
      R_USER = getwd()
    ), stderr = "2>&1",
    user_profile = FALSE
  )

  p_output <- consume_output_lines(p)
  expect_match(p_output, "Starting tessiflow scheduler")

  expect_gte(length(ps::ps_find_tree("tessiflow-daemon")), 1)

  p$kill_tree()
})

# tessiflow_disable -------------------------------------------------------

test_that("tessiflow_disable unschedules tessiflow", {
  unschedule_schtasks <- mock()
  unschedule_crontab <- mock()
  Platform <- .Platform
  stub(tessiflow_disable, "unschedule_schtasks", unschedule_schtasks)
  stub(tessiflow_disable, "unschedule_crontab", unschedule_crontab)

  Platform$OS.type <- "windows"
  assign(".Platform", Platform, envir = environment(tessiflow_disable))
  tessiflow_disable()

  Platform$OS.type <- "linux"
  assign(".Platform", Platform, envir = environment(tessiflow_disable))
  tessiflow_disable()

  expect_equal(mock_args(unschedule_schtasks)[[1]], list("tessiflow"))
  expect_equal(mock_args(unschedule_crontab)[[1]], list("tessiflow"))
})
