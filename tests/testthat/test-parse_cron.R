test_that("parse_cron_value understands numbers", {
  expect_equal(parse_cron_value(1), 1)
  expect_equal(parse_cron_value("1.0"), 1)
  expect_equal(parse_cron_value("10"), 10)
})

test_that("parse_cron_value understands months", {
  expect_equal(Vectorize(parse_cron_value, USE.NAMES = FALSE)(month.abb), seq(1, 12))
})

test_that("parse_cron_value understands weekdays", {
  expect_equal(Vectorize(parse_cron_value, USE.NAMES = FALSE)(c("Mon", "Tue", "wed", "thu", "fri", "sat", "sun")), seq(1, 7))
})

test_that("parse_cron_value complains when it doesn't undertand", {
  expect_error(parse_cron_value("huh"))
  expect_error(parse_cron_value(NA))
  expect_error(parse_cron_value(-1))
  expect_error(parse_cron_value(NULL))
})


# parse_cron_range --------------------------------------------------------

test_that("parse_cron_range handles single values", {
  expect_equal(parse_cron_range("5"), 5)
  expect_equal(parse_cron_range("mon"), 1)
  expect_equal(parse_cron_range("dec"), 12)
})

test_that("parse_cron_range handles ranges", {
  expect_equal(parse_cron_range("5-50"), seq(5, 50))
  expect_equal(parse_cron_range("mon-fri"), seq(1, 5))
  expect_equal(parse_cron_range("dec-mar"), seq(12, 3))
})


# parse_cron_part ---------------------------------------------------------

test_that("parse_cron_part understands '*' with or without '/'", {
  expect_equal(parse_cron_part("*"), seq(0, 59))
  expect_equal(parse_cron_part("*/4"), seq(0, 59, 4))
  expect_equal(parse_cron_part("*/4", c(3, 25)), seq(3, 25, 4))
})

test_that("parse_cron_part understands ',' with or without '-'", {
  expect_equal(parse_cron_part("3,5"), c(3, 5))
  expect_equal(parse_cron_part("3-7,5-9"), seq(3, 9))
  expect_equal(parse_cron_part("3-7,15-19"), c(seq(3, 7), seq(15, 19)))
  expect_equal(parse_cron_part("jan-mar,fri"), c(seq(1, 3), 5))
})

test_that("parse_cron_part understands '-' with or without '/'", {
  expect_equal(parse_cron_part("3-15"), seq(3, 15))
  expect_equal(parse_cron_part("3-15/3"), seq(3, 15, 3))
  expect_equal(parse_cron_part("jan-dec/2"), seq(1, 12, 2))
})

test_that("parse_cron_part complains about other combinations of operators", {
  expect_error(parse_cron_part("15,*"))
  expect_error(parse_cron_part("3-15,2-14/3"))
  expect_error(parse_cron_part("*-2"))
})


# parse_cron -------------------------------------------------------

withr::local_package("lubridate")
mockery::stub(parse_cron, "now", as.POSIXct("2020-01-01 00:00:00", Sys.timezone()))
mockery::stub(parse_cron, "today", as.Date("2020-01-01"))

test_that("parse_cron requires cron strings with 5 components", {
  expect_error(parse_cron("a b c d"))
  expect_error(parse_cron("a b c d e f"))
  expect_silent(parse_cron("1 * * * *"))
})

test_that("parse_cron retuns next two dates when wday not specified", {
  cron_string <- "30 4-7 1 jan-mar/2 *"
  expect_length(parse_cron(cron_string), 2)
  expect_equal(minute(parse_cron(cron_string)), c(30, 30))
  expect_equal(hour(parse_cron(cron_string)), c(7, 4))
  expect_equal(day(parse_cron(cron_string)), c(1, 1))
  expect_equal(month(parse_cron(cron_string)), c(3, 1))
})

test_that("parse_cron retuns next two dates when day not specified", {
  cron_string <- "30 4 * jan-dec/2 mon-fri/2"
  expect_length(parse_cron(cron_string), 2)
  expect_equal(minute(parse_cron(cron_string)), c(30, 30))
  expect_equal(hour(parse_cron(cron_string)), c(4, 4))
  expect_equal(wday(parse_cron(cron_string)), c(5, 5))
  expect_equal(month(parse_cron(cron_string)), c(11, 1))
})

test_that("parse_cron retuns next two dates when day and wday specified", {
  cron_string <- "30 4 1,15 * 5"
  expect_length(parse_cron(cron_string), 2)
  expect_equal(minute(parse_cron(cron_string)), c(30, 30))
  expect_equal(hour(parse_cron(cron_string)), c(4, 4))
  expect_true(all(wday(parse_cron(cron_string)) == 5 |
    day(parse_cron(cron_string)) %in% c(1, 15)))
})
