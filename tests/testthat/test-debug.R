withr::local_package("checkmate")
withr::local_package("mockery")
filename <- file.path(tempdir(),"flowname_jobname_12345.debug")
filename2 <- file.path(tempdir(),"flowname_jobname_54321.debug")
withr::defer(file.remove(filename))
withr::defer(file.remove(filename2))

test_that("preserve_debug_frames saves debug frames", {
  r_session <- r_session$new()
  
  filename <<- file.path(tempdir(),"flowname_jobname_12345.debug")
  expect_failure(expect_file_exists(filename))
  
  withr::local_options(callr.traceback = T)
  expect_error(r_session$run(readLines,list("notafile"), package = T), "cannot open the connection")
  r_session$run(preserve_debug_frames,list(filename = filename))  
  
  expect_file_exists(filename)

  debug_frames <- readRDS(filename)
  
  expect_gt(length(debug_frames), 1)
})

test_that("rehydrate_debug_frames restores debug frames", {
  r_session <- r_session$new(options = r_session_options(stdout = "|", stderr = "|"))
  r_session$run(function() {r <<- callr::r_session$new()})
  
  r_session$run(function(filename) {r$run(tessiflow:::rehydrate_debug_frames,list(filename = filename),package="tessiflow")},list(filename))
  r_session$call(function() {r$debug()})

  r_session$poll_io(1000)
  output <- r_session$read_output()
  expect_match(output,"Debugging in process")

  r_session$poll_io(1000)
  output <- r_session$read_output()
  expect_match(output, 'inspect a frame')
    
  r_session$poll_io(1000)
  output <- r_session$read_output()
  expect_match(output, 'file\\(con, "r"\\)')

})

test_that("tessiflow_debug complains if tessiflow.debug is not set", {
  expect_error(tessiflow_debug("flowname","jobname",12345),"tessiflow.debug is not set")
})

test_that("tessiflow_debug loads the corresponding debug file if pid given", {
  stub(tessiflow_debug,"config::get",tempdir)
  stub(tessiflow_debug,"rehydrate_debug_frames",function(filename){stop(filename)})
  expect_error(tessiflow_debug("flowname","jobname",12345),"flowname_jobname_12345.debug")
})

test_that("tessiflow_debug loads the latest debug file if pid == NULL", {
  stub(tessiflow_debug,"config::get",tempdir)
  stub(tessiflow_debug,"rehydrate_debug_frames",function(filename){stop(filename)})
  file.create(filename2)
  expect_error(tessiflow_debug("flowname","jobname"),"flowname_jobname_54321.debug")
})

test_that("tessiflow_debug complains if a debug file can't be found", {
  stub(tessiflow_debug,"config::get",tempdir)
  expect_error(tessiflow_debug("flowname","notajob",12345),"debug frames.+not found")
})
