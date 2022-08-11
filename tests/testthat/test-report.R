withr::local_package("lubridate")
withr::local_package("mockery")
withr::local_package("checkmate")

local_log_dir()
local_flows_data_table()
tessiflow$flows$start_time <- seq(now(tzone = Sys.timezone()) - dhours(36), now() - dseconds(1), dhours(6))
flows_log_upsert(data = tessiflow$flows)


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
  expect_match(mock_args(send_email)[[1]]$subject, "tessiflow")
  expect_match(mock_args(send_email)[[1]]$body, paste0("table.+Job 1.+Job 2.+Job 3.+", today(), ".+6H 0M 0S"))
})
