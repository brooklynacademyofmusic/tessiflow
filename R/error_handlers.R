#' error_handler
#'
#' functions for handling errors
#'
#' @describeIn error_handler selects error handler to use
#' @param error error condition
error_handler <- function(error) {
  if (!interactive()) {
    if (!is.null(config::get("tessiflow.email")) && !is.null(config::get("tessiflow.smtp"))) {
      error_email(error)
    } else {
      rlang::warn("Set tessiflow.email and tessiflow.smtp to allow emailing of messages")
    }
  }
  # in any case, print to the console
  error_print(error)
}

#' @describeIn error_handler prints an error message to the console
#' @importFrom cli cat_line
error_print <- function(error) {
  cli::cat_line(rlang::cnd_message(error, prefix = TRUE))
  print(error$trace)
}

#' @describeIn error_handler sends an error message to an email address
#' @importFrom checkmate test_character test_list
#' @importFrom cli ansi_html_style ansi_html
error_email <- function(error) {
  process_name <- "tessiflow"

  if ("flow_name" %in% names(error) || "job_name" %in% names(error)) {
    process_name <- paste(process_name, ":", error$flow_name, "/", error$job_name)
  }

  subject <- paste(process_name, "[ERROR]")
  body <- paste(c(
    "<style type='text/css'>",
    format(cli::ansi_html_style()),
    "</style><p>",
    process_name, "reported the following error:<p><pre>",
    cli::ansi_html(rlang::cnd_message(error, prefix=T)),"<br>",
    paste(cli::ansi_html(format(error$trace)),collapse="<br>")
  ), collapse = " ")
  
  send_email(subject = subject, body = body)
}

#' @param fun function to wrap
#' @describeIn error_handler wrap a function in an error handler
#' @importFrom rlang cnd_entrace env_parent call_match call_args
error_handler_factory <- function(fun) {
  function(...) {
    tryCatch(
      fun(...),
      error = function(e) {
        e <- cnd_entrace(e, bottom = env_parent())
        args <- call_args(call_match(quote(fun(...)), fun, dots_env = env_parent()))
        e$flow_name <- eval(args$flow_name)
        e$job_name <- eval(args$job_name)

        if (!is.null(e$flow_name) && !is.null(e$job_name)) {
          job_on_error(e$flow_name, e$job_name, e)
        } else {
          error_handler(e)
        }
      }
    )
  }
}
