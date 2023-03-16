#' error_handler
#' @title Error handlers
#' @description functions for handling errors
#' @describeIn error_handler selects error handler to use
#' @param error error condition
#' @importFrom rlang cnd_entrace
error_handler <- function(error) {
  error = cnd_entrace(error)
  
  try(if (!interactive()) {
    if (!is.null(config::get("tessiflow.email")) && !is.null(config::get("tessiflow.smtp"))) {
      error_email(error)
    } else {
      rlang::warn("Set tessiflow.email and tessiflow.smtp to allow emailing of messages")
    }
  })
  # in any case, print to the console
  error_print(error)
  
}

#' @describeIn error_handler prints an error message to the console
#' @importFrom cli cat_line
error_print <- function(error) {
  cli::cat_line(rlang::cnd_message(error, prefix = TRUE))
  print(error$trace)
  print(error$trace$call)
}

#' @describeIn error_handler sends an error message to an email address
#' @importFrom checkmate test_character test_list
#' @importFrom cli ansi_html_style ansi_html
error_email <- function(error) {
  process_name <- "tessiflow"

  withr::local_options(cli.num_colors = 255)

  if ("flow_name" %in% names(error) || "job_name" %in% names(error)) {
    process_name <- paste(process_name, ":", error$flow_name, "/", error$job_name)
  }

  subject <- paste(process_name, "[ERROR]")
  body <- paste(c(
    "<style type='text/css'>",
    format(cli::ansi_html_style()),
    "</style><p>",
    process_name, "reported the following error:<p><pre>",
    cli::ansi_html(rlang::cnd_message(error, prefix = TRUE)), "<br/>",
    paste(cli::ansi_html(format(error$trace)), collapse = "<br/>"), "</p><p>",
    paste(gsub("\\n","<br/>",error$trace$call), collapse = "<br/>"), "</p>"
  ), collapse = " ")

  send_email(subject = subject, body = body)
}

#' @param error error caught
#' @describeIn error_handler global error condition handler, intended to be used with `rlang::try_fetch` because it both adds trace information (as a calling handler)
#' and does error logging (as an error handler)
#' @importFrom rlang cnd_entrace env_parent call_match call_args
error_calling_handler <- function(error) {
    rlang::try_fetch({
      e <- cnd_entrace(error)
      error <- cnd_entrace(error,top = globalenv())
      call_stack <- error$trace$call %>% purrr::imap(~rlang::call_match(.x,sys.function(.y)))
      call_args <- purrr::map(call_stack,call_args)
      call_match <- purrr::detect_index(call_args, ~all(c("job_name","flow_name") %in% names(.)))
      if(call_match>0) {
        e$flow_name <- call_args[[call_match]]$flow_name
        e$job_name <- call_args[[call_match]]$job_name
      }
  
      if (!is.null(e$flow_name) && !is.null(e$job_name)) {
        job_on_error(e$flow_name, e$job_name, e)
      } else {
        error_handler(e)
      }
    }, error = function(e) {
      # minimal fallback
      e <- cnd_entrace(error)
      error_handler(e)
    })
}
