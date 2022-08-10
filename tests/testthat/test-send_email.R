withr::local_package("mockery")

test_that("send_email sends an email", {
  stub(send.mail, ".jTryCatch", function(...) {
    rlang::warn(class = "sent!")
  })
  stub(send_email, "send.mail", send.mail)
  stub(send_email, "config::get", mock("test@test.com", list(host.name = "blah"), cycle = TRUE))
  expect_warning(send_email("subject","body"), class = "sent!")
})