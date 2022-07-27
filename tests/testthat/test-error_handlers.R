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

test_that("error_handler_factory wraps functions with some rlang traceback sugar", {
  error_function <- function() stop("Bad fun")
  stub(error_handler_factory, "error_handler", error_print)
  wrapped_function <- error_handler_factory(error_function)
  expect_error(error_function())
  error_str <- "Error.+wrapped_function\\(\\)"
  expect_output(wrapped_function(), error_str)
})

test_that("error_print prints the error", {
  error <- quote(rlang::abort("I am an error"))
  expect_output(tryCatch(eval(error), error = error_print), "I am an error")
})

test_that("error_email emails the error", {
  stub(send.mail, ".jTryCatch", function(...) {
    rlang::warn(class = "sent!")
  })
  stub(error_email, "send.mail", send.mail)
  stub(error_email, "config::get", mock("test@test.com", list(host.name = "blah"), cycle = TRUE))
  error <- quote(rlang::abort("I am an error"))
  expect_warning(tryCatch(eval(error), error = error_email), class = "sent!")
})

test_that("error_email reads flow_name and job_name", {
  send_mail <- mock(0)
  stub(error_email, "send.mail", send_mail)
  stub(error_email, "config::get", mock("test@test.com", list(host.name = "blah"), cycle = TRUE))
  error <- quote(rlang::abort("I am an error", flow_name = "something", job_name = "something else"))
  tryCatch(eval(error), error = error_email)
  expect_length(mock_args(send_mail), 1)
  expect_match(mock_args(send_mail)[[1]][["subject"]], "something.+something else")
  expect_match(mock_args(send_mail)[[1]][["body"]], "something.+something else")
})

test_that("error_email reads flow_name and job_name from error_handler_factory", {
  send_mail <- mock()
  stub(error_email, "send.mail", send_mail)
  stub(error_email, "config::get", mock("test@test.com", list(host.name = "blah"), cycle = TRUE))
  stub(error_handler_factory, "error_email", error_email)
  stub(error_handler_factory, "job_on_error", function(flow_name, job_name, error) {
    error_email(error)
  })

  f <- error_handler_factory(function(flow_name, job_name) {
    stop("I am an error")
  })

  f("something", "something else")

  expect_length(mock_args(send_mail), 1)
  expect_match(mock_args(send_mail)[[1]][["subject"]], "something.+something else")
  expect_match(mock_args(send_mail)[[1]][["body"]], "something.+something else")
})
