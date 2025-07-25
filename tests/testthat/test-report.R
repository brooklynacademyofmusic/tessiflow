withr::local_package("lubridate")
withr::local_package("mockery")
withr::local_package("checkmate")

local_log_dir()
local_flows_data_table()
tessiflow$flows$start_time <- seq(now(tzone = Sys.timezone()) - dhours(30) - dseconds(1), now(), dhours(6))
sqlite_upsert(data = tessiflow$flows)


# tessiflow_report_load ---------------------------------------------------

test_that("tessiflow_report_load reads the last day's run info from the job database", {
  expect_equal(nrow(tessiflow_report_load()), 4)
  expect_equal(tessiflow_report_load()$`Start Time`, tessiflow$flows$start_time[3:6])
})

test_that("tessiflow_report_load adds `status` and `elapsed` columns", {
  expect_names(colnames(tessiflow_report_load()), must.include = c("Status", "Elapsed"))
})

# tessiflow_report_send ---------------------------------------------------

test_that("tessiflow_report_send emails the report", {
  send_email <- mock()
  stub(tessiflow_report_send, "send_email", send_email)
  tessiflow_report_send()
  expect_length(mock_args(send_email), 1)
  expect_match(subject <- mock_args(send_email)[[1]]$subject, "tessiflow")
  expect_match(body <- mock_args(send_email)[[1]]$body, paste0(
    "table.+>Job 1<.+>12H 0M.+>Job 2<.+>6H 0M.+>Job 3.+>",
    today()
  ))
})
