withr::local_package("mockery")
withr::local_package("checkmate")

local_log_dir()
local_flows_data_table()


# flows_main --------------------------------------------------------------

flows <- tessiflow$flows
test_that("flows_main does nothing when all tasks are finished", {
  flows[, status := "Finished"]
  stub(flows_main, "flows_parse", flows)
  stub(flows_main, "flows_main_read_server", close)
  m <- mock()
  stub(flows_main, "job_maybe_start_resilient", m)
  stub(flows_main, "job_poll_resilient", m)
  flows_main()
  expect_equal(length(mock_args(m)), 0)
})

test_that("flows_main calls job_maybe_start_resilient when tasks are waiting", {
  flows[, status := "Waiting"]
  stub(flows_main, "flows_parse", flows)
  m <- mock()
  stub(flows_main, "job_maybe_start_resilient", m)
  stub(flows_main, "flows_main_read_server", function(...) {
    close(...)
    stop("first loop")
  })
  expect_error(flows_main(), "first loop")
  expect_equal(length(mock_args(m)), nrow(flows))
})

test_that("flows_main calls job_poll_resilient when tasks are running", {
  flows[, status := "Running"]
  stub(flows_main, "flows_parse", flows)
  m <- mock()
  stub(flows_main, "job_poll_resilient", m)
  stub(flows_main, "flows_main_read_server", function(...) {
    close(...)
    stop("first loop")
  })
  expect_error(flows_main(), "first loop")
  expect_equal(length(mock_args(m)), nrow(flows))
})

test_that("flows_main resets tasks to waiting when they are done", {
  flows[, status := "Running"]
  stub(flows_main, "flows_parse", flows)
  m <- mock()
  stub(flows_main, "job_maybe_start_resilient", m)
  stub(flows_main, "job_poll_resilient", function(...) {
    tessiflow$flows[, `:=`(
      status = "Finished",
      scheduled_runs = list(list())
    )]
  })
  stub(flows_main, "flows_main_read_server", function(...) {
    close(...)
    stop("first loop")
  })
  expect_error(flows_main(), "first loop")
  expect_equal(flows, tessiflow$flows)
})


# flows_main_read_server --------------------------------------------------


test_that("flows_main_read_server reads from the server port if there is something to read and executes the command", {
  server <- serverSocket(32768)

  job_start <- mock()
  stub(flows_main_read_server, "job_start", job_start)

  socket <- socketConnection(port = 32768)
  cat(deparse(quote(job_start("flow_name", "job_name"))), file = socket, sep = "\n")
  expect_silent(flows_main_read_server(server))
  expect_length(mock_args(job_start), 1)
  close(socket)

  close(server)
})


test_that("flows_main_read_server returns a message if the string isn't a call or isn't allowed", {
  server <- serverSocket(32768)

  socket <- socketConnection(port = 32768)
  cat("do something", file = socket, sep = "\n")
  expect_message(flows_main_read_server(server), "Can't parse.+do something")
  close(socket)

  socket <- socketConnection(port = 32768)
  cat(deparse(quote(TRUE)), file = socket, sep = "\n")
  expect_message(flows_main_read_server(server), "Expression must be.+job_start.+job_stop.+TRUE")
  close(socket)

  socket <- socketConnection(port = 32768)
  cat(deparse(quote(stop("I am trying to make you fail!"))), file = socket, sep = "\n")
  expect_message(flows_main_read_server(server), "Expression must be.+job_start.+job_stop.+make you fail")
  close(socket)

  close(server)
})


# flows_get_job -----------------------------------------------------------

test_that("flows_get_job returns a list of job parameters", {
  job <- flows_get_job("Dummy workflow", "Job 1")
  expect_names(names(job), permutation.of = c(
    "flow_name", "job_name", "steps",
    "runs-on", "needs", "if", "env",
    "on.schedule", "scheduled_runs",
    "retval", "start_time", "end_time", "status"
  ))
})

test_that("flows_get_job gets the corresponding row from the flows table as a list", {
  flows <- readRDS(test_path("flows.Rds"))
  flow <- flows[[1]]
  flow$jobs$job1$`if` <- "TRUE"
  flow$jobs$job1$needs <- list("Job 2", "Job 3")
  stub(flows_parse, "yaml::read_yaml", mock(flow, NULL))

  tessiflow$flows <- flows_parse()

  job <- flows_get_job("Dummy workflow", "Job 1")

  expect_character(job$flow_name, len = 1)
  expect_character(job$job_name, len = 1)
  expect_character(job$`runs-on`, len = 1)
  expect_character(job$`if`, len = 1)

  expect_list(job$scheduled_runs, types = "POSIXct")
  expect_list(job$steps, types = c("character", "list"))
  expect_list(job$needs, types = "character")
  expect_list(job$env, types = "character", names = "named")
})

test_that("flows_get_job errors if it can't find the job", {
  expect_error(flows_get_job("not a workflow", "not a job"), "flow_name.+Dummy workflow")
})

# flows_update_job --------------------------------------------------------

test_that("flows_update_job updates the corresponding row in the flows table", {
  flows_update_job("Dummy workflow", "Job 1", list(status = "Running", start_time = now()))
  expect_equal(flows_get_job("Dummy workflow", "Job 1")$status, "Running")
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
})

test_that("flows_update_job updates the corresponding row in the flows database", {
  db <- flows_log_get_last_run("Dummy workflow", "Job 1")
  flows_update_job("Dummy workflow", "Job 1", list(status = "Finished", end_time = now()))
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$status, "Finished")
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$start_time, db$start_time)
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
})
