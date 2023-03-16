withr::local_package("mockery")

test_that("error_handler prints to console when interactive", {
  error_email <- mock()
  stub(error_handler, "interactive", TRUE)
  stub(error_handler, "error_email", error_email)
  error <- quote(rlang::abort("I am an error"))
  expect_output(tryCatch(eval(error), error = error_handler), "I am an error")
  expect_length(mock_calls(error_email), 0)
})

test_that("error_handler sends email when not interactive and configured", {
  error_email <- mock()
  stub(error_handler, "interactive", FALSE)
  stub(error_handler, "error_email", error_email)
  stub(error_handler, "config::get", NULL)
  error <- quote(rlang::abort("I am an error"))

  expect_output(
    expect_warning(
      tryCatch(eval(error), error = error_handler),
      "tessiflow.email.+tessiflow.smtp"
    ),
    "I am an error"
  )

  stub(error_handler, "config::get", TRUE)
  expect_output(tryCatch(eval(error), error = error_handler), "I am an error")
  expect_length(mock_calls(error_email), 1)
})


test_that("error_handler prints to console when not interactive", {
  error_email <- mock()
  stub(error_handler, "interactive", FALSE)
  stub(error_handler, "error_email", error_email)
  error <- quote(rlang::abort("I am an error"))
  stub(error_handler, "config::get", TRUE)
  expect_output(tryCatch(eval(error), error = error_handler), "I am an error")
  expect_length(mock_calls(error_email), 1)
})

test_that("error_print prints the error", {
  error <- quote(rlang::abort("I am an error"))
  expect_output(tryCatch(eval(error), error = error_print), "I am an error")
})

test_that("error_email reads flow_name and job_name", {
  send_email <- mock(0)
  stub(error_email, "send_email", send_email)
  error <- quote(rlang::abort("I am an error", flow_name = "something", job_name = "something else"))
  tryCatch(eval(error), error = error_email)
  expect_length(mock_args(send_email), 1)
  expect_match(mock_args(send_email)[[1]][["subject"]], "something.+something else")
  expect_match(mock_args(send_email)[[1]][["body"]], "something.+something else")
})

test_that("error_email reads flow_name and job_name from error_calling_handler", {
  send_email <- mock()
  stub(error_email, "send_email", send_email)
  stub(error_calling_handler, "error_email", error_email)
  stub(error_calling_handler, "job_on_error", function(flow_name, job_name, error) {
    error_email(error)
  })

  f <- function(flow_name, job_name) {
    stop("I am an error")
  }
  rlang::try_fetch(f("something", "something else"), error = error_calling_handler)

  expect_length(mock_args(send_email), 1)
  expect_match(mock_args(send_email)[[1]][["subject"]], "something.+something else")
  expect_match(mock_args(send_email)[[1]][["body"]], "something.+something else")
})

test_that("error_email returns formatted error and trace information", {
  stub(error_email, "send_email", function(body, ...) {
    return(body)
  })

  body <- tryCatch(sqrt(sum(1, "a")), error = rlang::cnd_entrace) %>% error_email()

  expect_match(body, "<span[^>]+ansi[^<]+Error")
  expect_match(body, "<span[^>]+ansi[^<]+tessiflow</span>:::error_email(.)")
})


# error_calling_handler ---------------------------------------------------

stub(error_handler, "interactive", TRUE)
stub(error_calling_handler,"error_handler",error_handler)

test_that("error_calling_handler finds the first function that has flow_name and job_name in it",{
  job_on_error <- mock()
  stub(error_calling_handler,"job_on_error", job_on_error)
  evil_function <- function(flow_name = NULL,job_name = NULL) { stop("I'm some kind of unpredictable error") }
  
  expect_output(rlang::try_fetch(stop(),error=error_calling_handler),"stop()")
  expect_output(rlang::try_fetch(evil_function(),error=error_calling_handler),"evil_function")
  expect_output(rlang::try_fetch(evil_function("flow name"),error=error_calling_handler),"evil_function.+flow_name")
  
  rlang::try_fetch(evil_function(flow_name = "flow name",job_name = "job_name"),error=error_calling_handler)
  rlang::try_fetch(evil_function("flow name","job_name"),error=error_calling_handler)
  
  expect_length(mock_args(job_on_error),2)
  
})

test_that("error_calling_handler works with errors at depth",{
  job_on_error <- mock()
  stub(error_calling_handler,"job_on_error",job_on_error)
  stub(job_log_write,"write",function(...){stop("Error in write")})

  job_test <- function() {job_log_write(flow_name = "Dummy workflow", job_name = "Job 1", "test")}
  rlang::try_fetch(job_test(),error=error_calling_handler)
  expect_length(mock_args(job_on_error),1)
  expect_equal(mock_args(job_on_error)[[1]][[3]]$message,"Error in write")
  expect_equal(mock_args(job_on_error)[[1]][[3]]$call[[1]],as.symbol("write"))
  
})

test_that("error_calling_handler gracefully handlers errors within itself",{
  stub(job_on_error,"assert_flow_job_name",function(...){stop("Inner error")})
  stub(error_calling_handler,"job_on_error",job_on_error)
  evil_function <- function(flow_name = NULL,job_name = NULL) { stop("Outer error") }
  
  expect_output(
    rlang::try_fetch(evil_function("flow_name","job_name"),error=error_calling_handler),
    "Outer error")
  expect_output(
    rlang::try_fetch(evil_function("flow_name","job_name"),error=error_calling_handler),
    "Inner error")
  
})

# test_that("error_handler_factory wraps functions with some rlang traceback sugar", {
#   error_function <- function() stop("Bad fun")
#   stub(error_handler_factory, "error_handler", error_print)
#   wrapped_function <- error_handler_factory(error_function)
#   expect_error(error_function())
#   error_str <- "Error.+wrapped_function\\(\\)"
#   expect_output(wrapped_function(), error_str)
# })

