

error_email <- function(error) {
  print(error)
  return()

  emails <- "ssyzygy@bam.org"
  subject <- paste0("tessitask [ERROR] (", Sys.info()["nodename"], ")")
  body <- paste("tessitask reported the following error:<p>", error, collapse = " ")

  send.mail(
    from = "ssyzygy@bam.org",
    to = emails,
    subject = subject,
    body = as.character(glue::glue(
      "Hi,
              {body}
            <p>Sincerely,
            <p>Sky's computer"
    )),
    smtp = list(host.name = "bam-org.mail.protection.outlook.com", port = 25),
    html = TRUE,
    send = TRUE
  )
}
