#' error_handler
#' 
#' functions for handling errors 
#' 
#' @describeIn error_handler selects error handler to use
#' @param error error condition
error_handler <- function(error) {
  if(!interactive()) {
    if(!is.null(config::get("tessiflow.email")) && !is.null(config::get("tessiflow.smtp"))) {
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
  cli::cat_line(rlang::cnd_message(error,prefix = TRUE))
  print(error$trace)
}

#' @param emails list of email addresses, default is from `tessiflow.email` configuration variable; `from` address will be the first email
#' @param smtp list of smtp configuration data for [`mailR::send.mail`]
#' @describeIn error_handler sends an error message to an email address
#' @importFrom mailR send.mail
#' @importFrom checkmate test_character test_list
#' @importFrom cli ansi_html_style ansi_html
error_email <- function(error,
                        emails = config::get("tessiflow.email"),
                        smtp = config::get("tessiflow.smtp")
                        ) {
  
  if(!test_character(emails,min.len=1))
    stop("Set tessiflow.email to the sender (first email) and list of recipients for error messages")
  if(!test_list(smtp))
    stop("Set tessiflow.smtp to the smtp server used to send error messages")
  
  process_name = "tessiflow"
  
  if("flow_name" %in% names(error) || "job_name" %in% names(error))
    process_name <- paste(process_name,":",error$flow_name,"/",error$job_name)
  
  subject <- paste0(process_name," [ERROR] (", Sys.info()["nodename"], ")")
  body <- paste(c("<style type='text/css'>",
                format(cli::ansi_html_style()),
                "</style><p>",
                process_name,"reported the following error:<p>", 
                cli::ansi_html(error)),collapse=" ")
  
  send.mail(
    from = emails[[1]],
    to = emails,
    subject = subject,
    body = body,
    smtp = smtp,
    html = TRUE,
    send = TRUE
  )
}

#' @param fun function to wrap
#' @describeIn error_handler wrap a function in an error handler
#' @importFrom rlang cnd_entrace env_parent call_match call_args
error_handler_factory <- function(fun) {
  
  function(...){
    tryCatch(
      fun(...),
      error = function(e) {
        e <- cnd_entrace(e,bottom=env_parent())
        args = call_args(call_match(quote(fun(...)),fun,dots_env=env_parent()))
        e$flow_name <- eval(args$flow_name)
        e$job_name <- eval(args$job_name)

        if(!is.null(e$flow_name) && !is.null(e$job_name)) {
          job_on_error(e$flow_name,e$job_name,e)
        } else {
          error_handler(e)
        }
      }
    )
  }
}