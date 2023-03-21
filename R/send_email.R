#' send_email
#'
#' Wrapper for [`mailR::send.mail`] that does input checking and pulls in configuration data from the `config.yml` file
#'
#' @param subject string subject line
#' @param body string body of email
#' @param emails list of email addresses, default is from `tessiflow.email` configuration variable; `from` address will be the first email
#' @param smtp list of `SMTP` configuration data for [`mailR::send.mail`]
#' @importFrom mailR send.mail
send_email <- function(subject, body,
                       emails = config::get("tessiflow.email"),
                       smtp = config::get("tessiflow.smtp")) {
  assert_character(subject, len = 1)
  assert_character(body, len = 1)

  if (!test_character(emails, min.len = 1)) {
    stop("Set tessiflow.email to the sender (first email) and list of recipients for error messages")
  }
  if (!test_list(smtp)) {
    stop("Set tessiflow.smtp to the smtp server used to send error messages")
  }

  send.mail(
    from = emails[[1]],
    to = emails,
    subject = paste0(subject, " (", Sys.info()["nodename"], ")"),
    body = body,
    smtp = smtp,
    encoding = "utf-8",
    html = TRUE,
    send = TRUE
  )
}
