withr::local_package("checkmate")
withr::local_package("mockery")
local_log_dir()

test_that("job_log_write complains if flow and job are not simple strings", {
  expect_error(job_log_write(1, c("a", "b")))
})

test_that("job_log_write writes lines to a file", {
  job_log_write("this is a flow", "this is a job", c("these", "are", "messages"))
  filename <- file.path(config::get("tessiflow.log"), "this is a flow.log")
  expect_file_exists(filename)
  expect_length(readLines(filename), 3)
})

test_that("job_log_write prepends job and timestamp to each line", {
  job_log_write("this is a flow", "this is a job", c("these", "are", "messages"))
  filename <- file.path(config::get("tessiflow.log"), "this is a flow.log")
  expect_true(all(grepl("\\d+-\\d+-\\d+ \\d+:\\d+:\\d+", readLines(filename))))
  expect_true(all(grepl("this is a job", readLines(filename))))
})

test_that("job_log_write keeps appending to the file", {
  job_log_write("this is a flow", "this is a job", c("these", "are", "messages"))
  filename <- file.path(config::get("tessiflow.log"), "this is a flow.log")
  expect_length(readLines(filename), 9)
})

test_that("job_log_write rotates a log file when it's over 1M", {
  filename <- file.path(config::get("tessiflow.log"), "this is a flow.log")
  zip_filename <- paste0(gsub(".log", "", filename, fixed = TRUE), "-", today(), ".zip")
  stub(job_log_write, "file.info", list(size = 2^20))

  job_log_write("this is a flow", "this is a job", c("these", "are", "messages"))
  expect_length(readLines(filename), 12)
  expect_false(file.exists(zip_filename))

  rm(job_log_write)
  stub(job_log_write, "file.info", list(size = 2^20 + 1))

  job_log_write("this is a flow", "this is a job", c("these", "are", "messages"))
  expect_length(readLines(filename), 3)
  expect_true(file.exists(zip_filename))
})

test_that("job_log_write prints to console when console = TRUE", {
  expect_message(job_log_write("this is a flow", "this is a job", "this is a message",console = TRUE),
                 "this is a message")
})
