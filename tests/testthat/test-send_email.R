withr::local_package("mockery")

# send_email --------------------------------------------------------------

test_that("send_email complains if subject, emails, or smtp are not set correctly", {
  sendmail <- mock()
  stub(send_email, "sendmail", sendmail)
  
  expect_error(send_email(),"subject.+missing")
  
  stub(send_email, "config::get", mock(NULL,
                                       "test@test.com", NULL,
                                       "test@test.com", list(host.name = "blah")))
  
  expect_error(send_email("subject", "body"),"email.+sender.+recipients")
  expect_error(send_email("subject", "body"),"smtp server")
})

test_that("send_email passes on parameters to sendmail", {
  sendmail <- mock()
  stub(send_email, "sendmail", sendmail)
  stub(send_email, "config::get", mock("test@test.com", list(hostname = "blah")))
  
  expect_silent(send_email("subject","body"))
  expect_length(mock_args(sendmail),1)
  expect_match(mock_args(sendmail)[[1]]$subject,"subject")
  expect_match(mock_args(sendmail)[[1]]$msg$text,"body")
  expect_equal(mock_args(sendmail)[[1]]$from,"test@test.com")
  expect_equal(mock_args(sendmail)[[1]]$to,"test@test.com")
  expect_equal(mock_args(sendmail)[[1]]$control,list(smtpServer="blah",smtpPort=NULL))
})

test_that("send_email sends an email", {
  stub(send_email, "config::get", mock("test@test.com", list(host.name = "blah"), cycle = TRUE))
  mail <- send_email(engine="debug","This is a test email")
  expect_match(mail,"This is a test email",all = F)
  expect_match(mail,"test@test.com",all = F)
  expect_match(mail,"Content-Type: text/html",all = F)
})
