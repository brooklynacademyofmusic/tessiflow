local_log_dir()

# tessiflow_start ---------------------------------------------------------

test_that("tessiflow_start starts a background process",{
  expect_length(ps::ps_find_tree("tessiflow_daemon"),0)
  tessiflow_start()
  expect_length(ps::ps_find_tree("tessiflow_daemon"),2)
})

test_that("tessiflow_start refuses to start if one is already running",{
 expect_error(tessiflow_start(),"Running tessiflow")
 expect_length(ps::ps_find_tree("tessiflow_daemon"),2)
})

test_that("tessiflow_start logs to a log file",{
 Sys.sleep(2)
 expect_gte(length(readLines(file.path(config::get("tessiflow.log"),"tessiflow.log"))),1)
})

# tessiflow_stop ----------------------------------------------------------

test_that("tessiflow_stop kills the daemon process",{
 expect_length(ps::ps_find_tree("tessiflow_daemon"),2)
 tessiflow_stop()
 expect_length(ps::ps_find_tree("tessiflow_daemon"),0)
})

test_that("tessiflow_stop kills all running jobs",{})

# tessiflow_enable --------------------------------------------------------

test_that("tessiflow_enable adds tessiflow to schtasks on Windows",{})

test_that("tessiflow_enable adds tessiflow to cron on *nix",{})

# tessiflow_disable -------------------------------------------------------

test_that("tessiflow_enable removes tessiflow from schtasks on Windows",{})

test_that("tessiflow_enable removes tessiflow from cron on *nix",{})

# tessiflow_job_start -----------------------------------------------------

test_that("tessiflow_job_start writes to the tessiflow input file/socket",{})

test_that("tessiflow_job_start starts a job",{})

# tessiflow_job_stop -----------------------------------------------------

test_that("tessiflow_job_stop writes to the tessiflow input file/socket",{})

test_that("tessiflow_job_stop stops a job",{})
