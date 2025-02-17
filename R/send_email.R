#' send_email
#'
#' Wrapper for [`sendmailR::sendmail`] that does input checking and pulls in configuration data from the `config.yml` file
#'
#' @param subject string subject line
#' @param body string body of email
#' @param emails list of email addresses, default is from `tessiflow.email` configuration variable; `from` address will be the first email
#' @param smtp named list, should contain `hostname` and (optionally) `port`, which defaults to 25.
#'  Additional elements will get merged with `...` and passed to curl, see [curl::curl_options] for more details.
#' @param ... additional parameters passed on to [sendmailR::sendmail]
#' @importFrom sendmailR sendmail mime_part_html
#' @importFrom checkmate assert assert_character check_character check_list test_character test_list
#' @importFrom purrr keep_at
#' @inheritParams sendmailR::sendmail
send_email <- function(subject, body = paste("Sent by", Sys.info()["nodename"]),
                       emails = config::get("tessiflow.email"),
                       smtp = config::get("tessiflow.smtp"),
                       engine = "curl",
                       ...
) {
  assert_character(subject, len = 1)
  assert(
    check_character(body, len = 1),
    check_list(body, "mime_part")
  )

  if (!test_character(emails, min.len = 1)) {
    stop("Set tessiflow.email to the sender (first email) and list of recipients for messages")
  }
  if (!test_list(smtp)) {
    stop("Set tessiflow.smtp to a list containing the `hostname` and optionally `port` of the smtp server")
  }
  
  if (typeof(body) == "character")
    body <- mime_part_html(body)
  
  dots <- modifyList(list(...),list(
    from = emails[[1]],
    to = emails,
    subject = subject,
    msg = body,
    control = list(smtpServer=smtp$hostname,smtpPort=smtp$port),
    engine = engine,
    engineopts = keep_at(smtp,names(curl::curl_options()))))
  
  do.call(sendmail,dots)
}
