withr::local_package("mockery")
withr::local_package("devtools")
local_config_file()

# tessiflow_run ---------------------------------------------------------

run_fun <- function() {
  local_log_dir(envir = new.env())
  mockery::stub(tessiflow_run, "flows_main", function() {
    Sys.sleep(10) # has to be long enough to allow the process to persist between tests
  })
  mockery::stub(tessiflow_run, "performance_main", TRUE)
  mockery::stub(tessiflow_run, "callr::r_bg", callr::r)
  tessiflow_run()
}

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

test_that("tessiflow_run logs to a log file", {
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")), 0)
  local_log_dir()

  stub(tessiflow_run, "flows_main", function() {
    message("Running flows_main()")
  })
  stub(tessiflow_run, "performance_main", function() {
    message("Running performance_main()")
  })

  suppressMessages(expect_message(tessiflow_run(), "Running flows_main", all = FALSE))

  logdata <- readLines(file.path(config::get("tessiflow.log"), "tessiflow-daemon.log"))
  expect_match(logdata, "Starting tessiflow", all = FALSE)
  expect_match(logdata, "Running flows_main", all = FALSE)
  expect_equal(length(logdata), 2)
})

# tessiflow_stop ----------------------------------------------------------

test_that("tessiflow_stop kills all running jobs", {
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

  expect_match(as.character(mock_args(schedule_schtasks)[[1]][[1]]), "tessiflow::tessiflow_run\\(\\)", all = FALSE)
  expect_match(as.character(mock_args(schedule_crontab)[[1]][[1]]), "tessiflow::tessiflow_run\\(\\)", all = FALSE)
})

test_that("tessiflow_enable schedules a runnable script", {
  local_log_dir()
  schedule_schtasks <- mock()
  Platform <- .Platform
  stub(tessiflow_enable, "Sys.getenv", getwd())
  stub(tessiflow_enable, "schedule_schtasks", schedule_schtasks)

  Platform$OS.type <- "windows"
  assign(".Platform", Platform, envir = environment(tessiflow_enable))
  tessiflow_enable()

  # create a config.yml file to mimic what happens in a normal install
  withr::local_file(list("config.yml" = yaml::write_yaml(
    list(default = list(
      tessiflow.log = config::get("tessiflow.log"),
      tessiflow.d = config::get("tessiflow.d")
    )),
    "config.yml"
  )))

  p <- callr::r_bg(system, list(mock_args(schedule_schtasks)[[1]][[1]]),
    env = c(R_CONFIG_FILE = "config.yml"), stderr = "2>&1"
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

# tessiflow_run_command -----------------------------------------------------

test_that("tessiflow_run_command errors if there's no running tessiflow process", {
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")), 0)
  expect_error(tessiflow_run_command("Dummy workflow", "Job 1", "this_is_a_function"), "No running tessiflow process")
})

test_that("tessiflow_run_command writes to the tessiflow input file/socket", {
  run_fun <- function() {
    local_log_dir(envir = new.env())
    mockery::stub(flows_main_read_server, "rlang::parse_expr", function(...) {
      return(print(...))
    })
    mockery::stub(flows_main, "flows_main_read_server", flows_main_read_server)
    print("Starting flows_main")
    flows_main()
  }

  withr::local_envvar("tessiflow-daemon" = "YES")

  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")), 0)

  p1 <- callr::r_bg(run_fun, package = "tessiflow", stderr = "2>&1")
  p1_output <- consume_output_lines(p1)
  expect_match(p1_output, "Starting flows_main")

  Sys.sleep(1)
  tessiflow_run_command("Dummy workflow", "Job 1", "this_is_a_function")
  p1_output <- consume_output_lines(p1)

  expect_match(p1_output, "this_is_a_function(.+Dummy workflow.+Job 1.+)")

  p1$kill_tree()
})
