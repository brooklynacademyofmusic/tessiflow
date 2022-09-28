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

test_that("error_email reads flow_name and job_name", {
  send_email <- mock(0)
  stub(error_email, "send_email", send_email)
  error <- quote(rlang::abort("I am an error", flow_name = "something", job_name = "something else"))
  tryCatch(eval(error), error = error_email)
  expect_length(mock_args(send_email), 1)
  expect_match(mock_args(send_email)[[1]][["subject"]], "something.+something else")
  expect_match(mock_args(send_email)[[1]][["body"]], "something.+something else")
})

test_that("error_email reads flow_name and job_name from error_handler_factory", {
  send_email <- mock()
  stub(error_email, "send_email", send_email)
  stub(error_handler_factory, "error_email", error_email)
  stub(error_handler_factory, "job_on_error", function(flow_name, job_name, error) {
    error_email(error)
  })

  f <- error_handler_factory(function(flow_name, job_name) {
    stop("I am an error")
  })

  f("something", "something else")

  expect_length(mock_args(send_email), 1)
  expect_match(mock_args(send_email)[[1]][["subject"]], "something.+something else")
  expect_match(mock_args(send_email)[[1]][["body"]], "something.+something else")
})

test_that("error_email returns formatted error and trace information", {
  withr::local_options(cli.num_colors = 255)

  stub(error_email, "send_email", function(body, ...) {
    return(body)
  })

  body <- tryCatch(sqrt(sum(1, "a")), error = rlang::cnd_entrace) %>% error_email()

  expect_match(body, "<span[^>]+ansi[^<]+Error")
  expect_match(body, "<span[^>]+ansi[^<]+tessiflow</span>:::error_email(.)")
})
