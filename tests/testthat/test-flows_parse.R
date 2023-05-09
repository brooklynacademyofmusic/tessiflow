withr::local_package("checkmate")
withr::local_package("mockery")

# test_parse --------------------------------------------------------------

test_that("test_parse returns false on non parseable strings", {
  expect_true(test_parse(NULL))
  expect_true(test_parse("TRUE"))
  expect_true(test_parse("if(this) {that}"))
  expect_false(test_parse("this is not a valid string"))
})

test_that("test_parse_run returns false on non parseable steps", {
  expect_true(test_parse_run(list()))
  expect_true(test_parse_run(list(`if` = "TRUE", run_expr = "print()", env = list(), shell = "callr")))
  expect_true(test_parse_run(list(`if` = NULL, run_expr = NULL, env = NULL, shell = NULL)))
  expect_false(test_parse_run(list(`if` = "TRUE TRUE", run_expr = "print()", env = list(), shell = "callr")))
  expect_false(test_parse_run(list(`if` = "TRUE", run_expr = "print()", env = "not an env", shell = "callr")))
  expect_false(test_parse_run(list(`if` = "TRUE", run_expr = "echo something", env = list(), shell = "callr")))
  expect_true(test_parse_run(list(`if` = "TRUE", run_expr = "echo something", env = list(), shell = "bash")))
})

# flows_parse -------------------------------------------------------------

test_that("flows_parse returns a data.table of workflows", {
  expect_data_table(flows_parse())
  expect_equal(nrow(flows_parse()), 6)
})

test_that("flows_parse errors when there aren't yml files in the directory or the directory doesn't exist", {
  expect_error(flows_parse("directorydoesntexist"), "yml files")
  expect_error(flows_parse(tempdir()), "yml files")
})

test_that("flows_parse reads name", {
  expect_equal(flows_parse()$flow_name, rep(c("Dummy workflow", "Dummy workflow 2"), each = 3))
})

test_that("flows_parse reads env", {
  expect_equal(flows_parse()$env, rep(list(list(env_variable = "value")), 6))
})

test_that("flows_parse reads on.schedule", {
  expect_equal(flows_parse()$on.schedule, rep(list(
    list(cron = "5 5,17 * * *"),
    list(
      cron = "5 5,17 * * *",
      cron = "30 4 * * 1"
    )
  ), each = 3))
})


test_that("flows_parse reads timeout", {
  expect_equal(flows_parse()$`timeout-minutes`, c(60,rep(list(NULL),5)))
})

test_that("flows_parse reads steps", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$jobs <- flows[[1]]$jobs[1]
  read_yaml <- mock(flows[[1]], flows[[2]], cycle = TRUE)
  stub(flows_parse, "yaml::read_yaml", read_yaml)

  expect_length(flows_parse()$steps[[1]], 2)
  expect_named(flows_parse()$steps[[1]][[1]], c("name","if","env","run"))
  expect_named(flows_parse()$steps[[1]][[2]], c("name","run","shell"))
})

test_that("flows_parse loads multiple files", {
  flows <- readRDS(test_path("flows.Rds"))
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  flows_parse()
  expect_length(mock_args(read_yaml), 2)
  expect_equal(basename(mock_args(read_yaml)[[1]][[1]]), "dummy.yml")
  expect_equal(basename(mock_args(read_yaml)[[2]][[1]]), "dummy2.yml")
})

test_that("flows_parse checks that flow names are unique", {
  flows <- readRDS(test_path("flows.Rds"))
  read_yaml <- mock(flows[[1]], flows[[1]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_error(flows_parse(), "flow.+unique")
})

test_that("flows_parse checks that job names are unique", {
  flows <- readRDS(test_path("flows.Rds"))
  names(flows[[1]]$jobs) <- c("job1", "job1", "job3")
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_error(flows_parse(), "Duplicated.+jobs")
})

test_that("flows_parse checks that step names are unique", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$jobs$job1$steps[[1]]$name <- flows[[1]]$jobs$job1$steps[[2]]$name
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_error(flows_parse(), "Duplicated step names")
})

test_that("flows_parse warns if there are keys we don’t know how to handle at the flow level", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$weird_key <- "weird_value"
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_warning(flows_parse(), "Invalid.+weird_key")
})

test_that("flows_parse warns if there are keys we don’t know how to handle at the job level", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$jobs[[1]]$weird_key <- "weird_value"
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_warning(flows_parse(), "Invalid.+weird_key")
})

test_that("flows_parse warns if there are no jobs", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$jobs <- NULL
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_warning(flows_parse(), "No jobs")
})

test_that("flows_parse warns if there is no schedule", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$`TRUE`$schedule <- NULL
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_warning(flows_parse(), "No schedule")
})

test_that("flows_parse errors if an `if` statement isn't parseable", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$jobs[[1]]$`if` <- "gobbledy gook"
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_error(flows_parse(), "gobbledy gook")
})

test_that("flows_parse errors if a run statement isn't parseable", {
  flows <- readRDS(test_path("flows.Rds"))
  flows[[1]]$jobs[[1]]$steps[[1]]$run <- "gobbledy gook"
  read_yaml <- mock(flows[[1]], flows[[2]])
  stub(flows_parse, "yaml::read_yaml", read_yaml)
  expect_error(flows_parse(), "gobbledy gook")
})


# flows_refresh -----------------------------------------------------------

test_that("flows_refresh updates the tessiflow data that doesn't involve run state", {
  local_flows_data_table()
  run_state_cols <- c("status", "retval", "start_time", "end_time")
  other_cols <- setdiff(colnames(tessiflow$flows), run_state_cols)
  
  tessiflow$flows[, (run_state_cols) := list(seq_len(.N), seq_len(.N), now() + seq_len(.N), now() + seq_len(.N))]
  
  new_data <- tessiflow$flows[c(1, 4, 6)][, job_name := c("Job 2", "Job 3", "New job")]
  old_data <- copy(tessiflow$flows)
  stub(flows_refresh, "flows_parse", new_data)
  flows_refresh()
  
  expect_equal(tessiflow$flows[c(1, 3, 4, 5)], old_data[c(1, 3, 4, 5)])
  expect_equal(tessiflow$flows[c(2, 6), ..other_cols], new_data[c(1, 2), ..other_cols])
})

test_that("flows_refresh adds additional flow data", {
  local_flows_data_table()
  
  new_data <- tessiflow$flows[c(1, 4, 6)][, job_name := c("Job 2", "Job 3", "New job")]
  stub(flows_refresh, "flows_parse", new_data)
  flows_refresh()
  
  expect_equal(tessiflow$flows[7], new_data[3])
})

test_that("flows_refresh adds new columns", {
  local_flows_data_table()
  
  new_data <- tessiflow$flows[c(1, 4, 6)][, job_name := c("Job 2", "Job 3", "New job")][, new_column := 1]
  stub(flows_refresh, "flows_parse", new_data)
  flows_refresh()
  
  expect_equal(tessiflow$flows[, new_column], c(rep(NA, 6), 1))
})

test_that("flows_refresh works when columns missing", {
  local_flows_data_table()
  
  new_data <- tessiflow$flows[c(1, 4, 6)][, job_name := c("Job 2", "Job 3", "New job")][, env := NULL]
  old_data <- copy(tessiflow$flows)
  stub(flows_refresh, "flows_parse", new_data)
  flows_refresh()
  
  expect_equal(tessiflow$flows$env, c(old_data$env, list(NULL)))
})

# flows_auto_refresh ------------------------------------------------------

test_that("flows_auto_refresh loads flows when they haven't been yet", {
  flows_parse <- mock(NULL,NULL)
  stub(flows_auto_refresh,"flows_parse",flows_parse)
  
  expect_message(flows_auto_refresh(),"Loading flows from")
  local_flows_data_table()
  tessiflow$flows_refresh_time <- NULL
  
  expect_message(flows_auto_refresh(),"Loading flows from")
  expect_length(mock_args(flows_parse),2)
  
  rm(flows_auto_refresh)
  time <- now()
  expect_message(flows_auto_refresh(),"Loading flows from")
  expect_gte(tessiflow$flows_refresh_time,time)
})

test_that("flows_auto_refresh only refreshes flows when a yml file has been updated", {
  local_flows_data_table()

  flows_refresh <- mock(flows_parse())
  stub(flows_auto_refresh,"flows_refresh",flows_refresh)
  
  expect_silent(flows_auto_refresh())
  expect_length(mock_args(flows_refresh),0)
  
  stub(flows_auto_refresh,"file.mtime",now())
  
  expect_message(flows_auto_refresh(),"Refreshing flows from")
  expect_length(mock_args(flows_refresh),1)
  
  expect_silent(flows_auto_refresh())
  expect_length(mock_args(flows_refresh),1)
})

