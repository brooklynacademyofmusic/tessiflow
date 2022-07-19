withr::local_package("mockery")
withr::local_package("checkmate")
withr::defer(flows_log_close())

local_log_dir()

test_that("flows_log_create creates a new database table", {
  stub(flows_log_open, "flows_log_create", TRUE)
  flows_log_open()
  flows_log_create()
  expect_true("jobs" %in% DBI::dbListTables(tessiflow$db))
})

test_that("flows_log_create creates an index on the table", {
  expect_equal(DBI::dbGetQuery(tessiflow$db, "pragma index_list(jobs);") %>% nrow(), 1)
})


# flows_log_open ----------------------------------------------------------

test_that("flows_log_open errors when the config option isn't set or the directory doesn't exist", {
  expect_error(flows_log_open("dirdoesntexist"), "config.+directory.+log")
  stub(flows_log_open, "config::get", "blah")
  expect_error(flows_log_open(), "config.+directory.+log")
})

test_that("flows_log_open opens a database connection", {
  flows_log_open()
  expect_class(tessiflow$db, "SQLiteConnection")
})




# flows_log_upsert --------------------------------------------------------

test_that("flows_log_upsert complains if the table isn't in the database or doesn't have the join columns", {
  expect_error(flows_log_upsert("notatable", c(x = 1)), "notatable")
  expect_error(flows_log_upsert("jobs", c(notafield = 1)), "notafield")
  expect_error(flows_log_upsert("jobs", data.frame(start_time = now())), "job_name,flow_name")
})

test_that("flows_log_upsert writes a new row to the table", {
  flows_log_upsert("jobs", data.frame(
    flow_name = "Dummy workflow",
    job_name = "Job 1",
    status = "Waiting",
    start_time = now(),
    end_time = NA,
    retval = NA
  ))
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
})

test_that("flows_log_upsert writes a row to the table", {
  job <- flows_log_get_last_run("Dummy workflow", "Job 1")
  job$status <- "Running"
  flows_log_upsert("jobs", job)
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 1)
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$status, "Running")
})

# flows_low_get_last_run --------------------------------------------------

flows_log_upsert("jobs", readRDS(test_path("jobs.Rds")))
test_that("flows_log_get_last_run reports the last run by flow and job", {
  start_time <- as.double(now())

  flows_log_upsert("jobs", data.frame(
    flow_name = "Dummy workflow",
    job_name = "Job 1",
    status = "Waiting",
    start_time = start_time,
    end_time = NA,
    retval = NA
  ))
  expect_equal(DBI::dbGetQuery(tessiflow$db, "select count(*) from jobs")[[1]], 8)
  expect_equal(flows_log_get_last_run("Dummy workflow", "Job 1")$start_time, start_time)
})
