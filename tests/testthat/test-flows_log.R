withr::local_package("mockery")
withr::local_package("checkmate")

local_log_dir()

test_that("flows_log_open errors when the config option isn't set or the directory doesn't exist",{
  expect_error(flows_log_open("dirdoesntexist"),"config.+directory.+log")
  stub(flows_log_open,"config::get","blah")
  expect_error(flows_log_open(),"config.+directory.+log")
})

test_that("flows_log_open opens a database connection",{
  flows_log_open()
  expect_class(tessiflow$db,"SQLiteConnection")
})

test_that("flows_log_open creates the jobs db table",{
  flows_log_open()
  expect_true(DBI::dbExistsTable(tessiflow$db,"jobs"))
})

# flows_log_write ---------------------------------------------------------

test_that("flows_log_write complains if the table isn't in the database or doesn't have the right columns",{
  expect_error(flows_log_write("notatable",c(x=1)),"notatable")
  expect_error(flows_log_write("jobs",c(notafield=1)),"notafield")
  expect_silent(flows_log_write("jobs",data.frame(start_time=now())))
})

test_that("flows_log_write writes a row to the table",{
  flows_log_write("jobs",data.frame(flow_name = "Dummy workflow",
                                   job_name = "Job 1",
                                   status = "Waiting",
                                   start_time = now(),
                                   end_time = NA,
                                   retval = NA))
  expect_equal(DBI::dbGetQuery(tessiflow$db,"select count(*) from jobs")[[1]],2)
})

# flows_low_get_last_run --------------------------------------------------

dplyr::copy_to(tessiflow$db,readRDS(test_path("jobs.Rds")),"jobs",overwrite = TRUE)
test_that("flows_log_get_last_run reports the last run by flow and job",{
  start_time = as.double(now())
  
  flows_log_write("jobs",data.frame(flow_name = "Dummy workflow",
                                    job_name = "Job 1",
                                    status = "Waiting",
                                    start_time = start_time,
                                    end_time = NA,
                                    retval = NA))
  expect_equal(flows_log_get_last_run("Dummy workflow","Job 1")$start_time,start_time)
})
