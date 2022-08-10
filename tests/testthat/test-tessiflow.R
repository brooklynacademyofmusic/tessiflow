withr::local_package("mockery")
withr::local_package("devtools")

# tessiflow_run ---------------------------------------------------------
run_expr <- quote({
  tessiflow:::local_log_dir(envir=new.env())
   tryCatch(devtools::load_all(quiet = TRUE),
            error = function(e) {library("tessiflow")})
  mockery::stub(tessiflow_run,"flows_main",function() {
    message("Running flows_main()")
    Sys.sleep(10) # has to be long enough to allow the process to persist between tests
  })
  tessiflow_run()
})

consume_output_lines <- function(process) {
  # consume the rest of the output lines
  process$poll_io(10000)
  while(length(process$read_output_lines())>0)
    Sys.sleep(1)
}

num_processes <- 0

test_that("tessiflow_run refuses to start if tessiflow is already running",{
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")),0)
  
  p1 <- callr::r_bg(eval,list(run_expr))
  p1$poll_io(10000)
  p1_output <- p1$read_output_lines()
  expect_match(p1_output,"Starting tessiflow")
  consume_output_lines(p1)
  num_processes <<- length(ps::ps_find_tree("tessiflow-daemon"))
  expect_gte(num_processes,1)
  
  p2 <- callr::r_bg(eval,list(run_expr))
  p2$poll_io(10000)
  p2_error <- p2$read_error_lines()
  expect_match(p2_error,"Found running tessiflow",all=FALSE)
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")),num_processes)
  
  p1$kill_tree()
  p2$kill_tree()
})

test_that("tessiflow_run logs to a log file",{
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")),0)
  
  p1 <- callr::r_session$new(callr::r_session_options(stderr="",stdout=""))
  logdir <- p1$run_with_output(eval,list(quote(config::get("tessiflow.log"))))$result
  p1$call(eval,list(run_expr))
  consume_output_lines(p1)
  
  expect_length(logdir,1) # message isn't printed to console
  logdata <- readLines(file.path(logdir,"tessiflow.log"))
  expect_match(logdata,"Starting tessiflow",all = FALSE)
  expect_match(logdata,"Running flows_main",all = FALSE)
  expect_equal(length(logdata),2) 
  
  p1$kill_tree()
})

# tessiflow_stop ----------------------------------------------------------

test_that("tessiflow_stop kills the daemon process",{
 p1 <- callr::r_bg(eval,list(run_expr))
 consume_output_lines(p1)
 
 expect_gte(length(ps::ps_find_tree("tessiflow-daemon")),1)
 tessiflow_stop()
 expect_equal(length(ps::ps_find_tree("tessiflow-daemon")),0)
})

test_that("tessiflow_stop kills all running jobs",{
  run_expr <- quote({
    tessiflow:::local_log_dir(envir=new.env())
    tryCatch(devtools::load_all(quiet = TRUE),
             error = function(e) {library("tessiflow")})
    mockery::stub(tessiflow_run,"flows_main",function() {
      callr::r(Sys.sleep,list(10))
    })
    tessiflow_run()
  })
  
  p1 <- callr::r_bg(eval,list(run_expr))
  consume_output_lines(p1)
  
  expect_gte(length(ps::ps_find_tree("tessiflow-daemon")),num_processes)
  
  tessiflow_stop()
  Sys.sleep(1)
  
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")),0)
})

# tessiflow_enable --------------------------------------------------------

test_that("tessiflow_enable schedules tessiflow",{
  schedule_schtasks <- mock()
  schedule_crontab <- mock()
  Platform <- .Platform
  stub(tessiflow_enable,"schedule_schtasks",schedule_schtasks)
  stub(tessiflow_enable,"schedule_crontab",schedule_crontab)  
  
  Platform$OS.type <- "windows"
  assign(".Platform",Platform,envir=environment(tessiflow_enable))
  tessiflow_enable()
  
  Platform$OS.type <- "linux"
  assign(".Platform",Platform,envir=environment(tessiflow_enable))
  tessiflow_enable()
  
  expect_equal(mock_args(schedule_schtasks)[[1]],list(quote(tessiflow::tessiflow_run()),"tessiflow"))
  expect_equal(mock_args(schedule_crontab)[[1]],list(quote(tessiflow::tessiflow_run()),"tessiflow"))
})

# tessiflow_disable -------------------------------------------------------

test_that("tessiflow_disable unschedules tessiflow",{
  unschedule_schtasks <- mock()
  unschedule_crontab <- mock()
  Platform <- .Platform
  stub(tessiflow_disable,"unschedule_schtasks",unschedule_schtasks)
  stub(tessiflow_disable,"unschedule_crontab",unschedule_crontab)  
  
  Platform$OS.type <- "windows"
  assign(".Platform",Platform,envir=environment(tessiflow_disable))
  tessiflow_disable()
  
  Platform$OS.type <- "linux"
  assign(".Platform",Platform,envir=environment(tessiflow_disable))
  tessiflow_disable()
  
  expect_equal(mock_args(unschedule_schtasks)[[1]],list("tessiflow"))
  expect_equal(mock_args(unschedule_crontab)[[1]],list("tessiflow"))
})

# tessiflow_run_command -----------------------------------------------------

test_that("tessiflow_run_command errors if there's no running tessiflow process",{
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")),0)
  expect_error(tessiflow_run_command("Dummy workflow","Job 1","this_is_a_function"),"No running tessiflow process")
})

test_that("tessiflow_run_command writes to the tessiflow input file/socket",{
  run_expr <- quote({
    tessiflow:::local_log_dir(envir=new.env())
    tryCatch(devtools::load_all(quiet = TRUE),
             error = function(e) {library("tessiflow")})
    mockery::stub(flows_main,"flows_main_read_server",function(server) {
      socket <- try(socketAccept(server,timeout=1),silent=TRUE)
      if(!"try-error" %in% class(socket)) {
        input <- readLines(socket,n=1)
        print(input)
      }
    })
    flows_main()
  })
  
  withr::local_envvar("tessiflow-daemon"="YES")
  expect_equal(length(ps::ps_find_tree("tessiflow-daemon")),0)
  
  p1 <- callr::r_bg(eval,list(run_expr))
  consume_output_lines(p1)

  tessiflow_run_command("Dummy workflow","Job 1","this_is_a_function")
  
  while(length(p1_output <- p1$read_output_lines())==0)
    Sys.sleep(1)
  
  expect_match(p1_output,"this_is_a_function(.+Dummy workflow.+Job 1.+)")
})
