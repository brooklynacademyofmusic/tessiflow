% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/send_email.R
\name{send_email}
\alias{send_email}
\title{send_email}
\usage{
send_email(
  subject,
  body = paste("Sent by", Sys.info()["nodename"]),
  emails = config::get("tessiflow.email"),
  smtp = config::get("tessiflow.smtp"),
  engine = "curl",
  ...
)
}
\arguments{
\item{subject}{string subject line}

\item{body}{string body of email}

\item{emails}{list of email addresses, default is from \code{tessiflow.email} configuration variable; \code{from} address will be the first email}

\item{smtp}{named list, should contain \code{hostname} and (optionally) \code{port}, which defaults to 25.
Additional elements will get merged with \code{...} and passed to curl, see \link[curl:curl_options]{curl::curl_options} for more details.}

\item{engine}{One of: \itemize{
\item{\code{"internal"} for the internal smtp transport (default).}
\item{\code{"curl"} for the use of curl for transport. Enable if you need STARTTLS/SSL
and/or SMTP authentication. See \code{curl::\link[curl]{send_mail}}.}
\item{\code{"debug"} sendmail returns a RFC2822 formatted email message without sending it.}
}}

\item{...}{additional parameters passed on to \link[sendmailR:sendmail]{sendmailR::sendmail}}
}
\description{
Wrapper for \code{\link[sendmailR:sendmail]{sendmailR::sendmail}} that does input checking and pulls in configuration data from the \code{config.yml} file
}
