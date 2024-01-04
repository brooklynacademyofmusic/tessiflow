withr::local_package("checkmate")
withr::local_package("mockery")
local_log_dir()

tessiflow$flows <- data.table(flow_name = "this is a flow", job_name = "this is a job")

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

test_that("job_log_write prints to console when console = TRUE", {
  expect_message(
    job_log_write("this is a flow", "this is a job", "this is a message", console = TRUE),
    "this is a message"
  )
})


# log_rotate --------------------------------------------------------------

test_that("log_rotate rotates a log file when it's over size=size", {
  filename <- tempfile()
  zip_filename <- paste0(gsub(".log", "", filename, fixed = TRUE), "-", format(now(),"%Y-%m-%d %H-%M-%S"), ".zip")

  write(c("these", "are", "lines"), filename, append = TRUE, sep = "\n")

  log_rotate(filename, size = file.info(filename)$size)
  expect_length(readLines(filename), 3)
  expect_false(file.exists(zip_filename))

  log_rotate(filename, size = file.info(filename)$size - 1)
  expect_false(file.exists(filename))
  expect_true(file.exists(zip_filename))
})

test_that("log_rotate doesn't clobber existing zip file or working log file", {
  filename <- tempfile()
  zip_filename <- paste0(gsub(".log", "", filename, fixed = TRUE), "-", format(now(),"%Y-%m-%d %H-%M-%S"), ".zip")
  
  write(c("these", "are", "lines"), filename, append = TRUE, sep = "\n")
  
  log_rotate(filename, size = 0)
  
  write(c("and", "some", "more"), filename, append = TRUE, sep = "\n")
  
  log_rotate(filename, size = 0)
  
  extract_dir <- tempfile()
  dir.create(extract_dir)
  utils::unzip(zip_filename,exdir = extract_dir)  
  files <- c(dir(extract_dir,full.names = T),filename)
  
  expect_equal(as.character(sapply(files[file.exists(files)],
                                   readLines)),
               c("these","are","lines","and","some","more"))
  
})
