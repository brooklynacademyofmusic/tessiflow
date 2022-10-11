withr::local_package("mockery")
# withr::defer(eval(rlang::expr(.Platform$OS.type <- !!.Platform$OS.type)))
# .Platform$OS.type <- "windows"

temp <- tempfile()
expr <- rlang::expr({
  writeLines("hello world!", !!temp)
})

if (.Platform$OS.type != "windows") {
  stub(schedule_schtasks, "system2", function(...) {
    "SUCCESS"
  })
  stub(unschedule_schtasks, "system2", function(...) {
    "SUCCESS"
  })
}

test_that("script_expr creates a runnable string", {
  withr::local_file(list(temp = file.create(temp)))
  system(script_expr(expr), intern = TRUE)
  expect_equal(readLines(temp), "hello world!")
})

test_that("schedule_schtasks runs successfully", {
  return <- schedule_schtasks(script_expr(expr), "dummy_task")
  expect_match(return, "SUCCESS")
})

test_that("schedule_schtasks creates a usable task", {
  withr::local_file(list(temp = file.create(temp)))
  if (.Platform$OS.type == "windows") {
    expect_match(system("schtasks /run /tn dummy_task", intern = TRUE), "SUCCESS")
  } else {
    eval(expr)
  }
  expect_equal(readLines(temp), "hello world!")
})

test_that("schedule_schtasks deletes the task", {
  return <- unschedule_schtasks("dummy_task")
  expect_match(return, "SUCCESS")
})

filename <- NULL

test_that("schedule_crontab adds the script to the crontab file", {
  system2 <- mock("this is a task already there", 0)
  stub(schedule_crontab, "system2", system2)
  schedule_crontab(script_expr(expr), "dummy_task")
  filename <<- mock_args(system2)[[2]][[2]]
  crontab <- readLines(filename)
  expect_length(crontab, 3)
  expect_match(crontab, "# dummy_task", all = FALSE)
  expect_match(crontab, "hello world!", all = FALSE)
})

test_that("schedule_crontab creates a usable task", {
  withr::local_file(temp)
  crontab <- readLines(filename)
  script <- gsub("* * * * *", "", tail(crontab, 1), fixed = TRUE)
  system(script, ignore.stdout = TRUE)
  expect_equal(readLines(temp), "hello world!")
})

test_that("unschedule_crontab removes the task from crontab", {
  system2 <- mock(readLines(filename), 0)
  stub(unschedule_crontab, "system2", system2)
  unschedule_crontab("dummy_task")
  filename <- mock_args(system2)[[2]][[2]]
  crontab <- readLines(filename)
  expect_length(crontab, 1)
})
