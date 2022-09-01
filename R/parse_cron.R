
#' parse_cron
#'
#' @param cron_string string with the standard *nix format, see Details for details
#' @details
#' The time and date fields are:
#'
#' **field**    |  **allowed values**
#' -----        |  --------------
#' minute       |  0-59
#' hour         |  0-23
#' day of month |  1-31
#' month        |  1-12 (or names, see below)
#' day of week  |  0-7 (0 or 7 is Sunday, or use names)
#'
#' A field may contain an asterisk (*), which always stands for
#' `first-last`.
#'
#' Ranges of numbers are allowed.  Ranges are two numbers separated
#' with a hyphen.  The specified range is inclusive.  For example,
#' 8-11 for an 'hours' entry specifies execution at hours 8, 9, 10,
#' and 11. The first number must be less than or equal to the second
#' one.
#'
#' Lists are allowed.  A list is a set of numbers (or ranges)
#' separated by commas.  Examples: `1,2,5,9`, `0-4,8-12`.
#'
#' Step values can be used in conjunction with ranges.  Following a
#' range with `/number` specifies skips of the number's value
#' through the range.  For example, `0-23/2` can be used in the
#' 'hours' field to specify command execution for every other hour
#' (the alternative in the V7 standard is `0,2,4,6,8,10,12,14,16,18,20,22`).
#' Step values are also permitted after an asterisk, so if
#' specifying a job to be run every two hours, you can use `*/2`.
#'
#' Names can also be used for the 'month' and 'day of week' fields.
#' Use the first three letters of the particular day or month (case
#' does not matter).  Ranges and lists of names are allowed.
#' Examples: `mon,wed,fri`, `jan-mar`.
#'
#' *Note:* The day of a command's execution can be specified by two fields -
#' day of month, and day of week. If both fields are restricted (ie, aren't *),
#' the command will be run when either field matches the current time. For example,
#' `30 4 1,15 * 5` would cause a command to be run at 4:30 am on the
#' 1st and 15th of each month, plus every Friday.
#'
#' @return last and next runtime
#' @importFrom checkmate assert_string
#' @importFrom lubridate make_date hours minutes now force_tz ceiling_date
#'
parse_cron <- function(cron_string) {
  . <- datetime <- NULL

  # split string into components
  cron_parts <- strsplit(cron_string, "\\s+", perl = TRUE)[[1]]

  if (!test_character(cron_parts, any.missing = FALSE, all.missing = FALSE, len = 5)) {
    stop(paste("Cron string", cron_string, "does not have five parts."))
  }

  names(cron_parts) <- c("min", "hour", "mday", "month", "wday")

  cron <- map2(cron_parts, list(
    c(0, 59),
    c(0, 23),
    c(1, 31),
    c(1, 12),
    c(0, 7)
  ), parse_cron_part)

  cron$year <- year(now()) %>% {
      seq(. - 1, . + 1)
    }

  ### Day parts
  cron_data_table <- data.table()
  # if weekday is restricted
  if (cron_parts["wday"] != "*") {
    cron_wday <- cron
    # ... unrestrict day
    cron_wday$mday <- seq(1, 31)
    cron_data_table <- expand.grid(cron_wday[c("year", "month", "mday", "wday")])
  }

  # if day is restricted (or weekday is not)
  if (cron_parts["mday"] != "*" || cron_parts["wday"] == "*") {
    cron_mday <- cron
    # ... unrestrict wday
    cron_mday$wday <- NA
    cron_data_table <- expand.grid(cron_mday[c("year", "month", "mday", "wday")]) %>%
      rbind(cron_data_table)
  }

  ### Filter dates
  cron_data_table <- cron_data_table %>%
    setDT() %>%
    .[, `:=`(
      date = make_date(year, month, mday),
      today = FALSE
    )] %>%
    .[is.na(wday) & !is.na(date) | # is a valid date
      !is.na(wday) & wday(date) == wday] %>% # is the right weekday
    rbind(data.table(date = today(), today = TRUE), fill = TRUE)

  setorder(cron_data_table, date, -today)
  today_row <- which(cron_data_table$today == TRUE)
  cron_data_table <- cron_data_table[c(today_row - 1, today_row + 1, today_row + 2), ]

  ### Time parts

  times <- expand.grid(cron[c("min", "hour")])

  cron_data_table <- merge.data.frame(cron_data_table, times) %>%
    setDT() %>%
    .[, `:=`(
      datetime = lubridate::force_tz(date + hours(hour) + minutes(min), Sys.timezone()),
      now = FALSE
    )] %>%
    rbind(data.table(datetime = ceiling_date(now(), "minutes"), now = TRUE), fill = TRUE)

  setorder(cron_data_table, datetime, -now)
  now_row <- which(cron_data_table$now == TRUE)
  cron_data_table <- cron_data_table[c(now_row - 1, now_row + 1), ]

  return(na.omit(cron_data_table$datetime))
}

#' parse_cron_part
#' parse one part of the cron string
#'
#' @param cron_part string part of the cron string
#' @param range vector of integers indicating the range of the cron part
#'
#' @return vector of integers of allowed date values
#' @importFrom checkmate assert_integer
parse_cron_part <- function(cron_part, range = c(0, 59)) {
  if (grepl(",", cron_part) && grepl("/", cron_part)) {
    stop(paste0(cron_part, ": Invalid cron string, don't know how to handle ',' and '/' in the same part."))
  }
  if (grepl(",", cron_part) && grepl("*", cron_part, fixed = TRUE)) {
    stop(paste0(cron_part, ": Invalid cron string, don't know how to handle ',' and '*' in the same part."))
  }
  if (grepl("*", cron_part, fixed = TRUE) && grepl("-", cron_part)) {
    stop(paste0(cron_part, ": Invalid cron string, don't know how to handle '*' and '-' in the same part."))
  }

  step <- 1

  if (grepl(",", cron_part)) {
    values <- strsplit(cron_part, ",", fixed = TRUE)[[1]]
    return(unique(unlist(lapply(values, parse_cron_range))))
  } else {
    values <- cron_part
    if (grepl("/", cron_part)) {
      values <- strsplit(cron_part, "/", fixed = TRUE)[[1]]
      step <- suppressMessages(as.integer(values[2]))
      if (!checkmate::test_int(step, lower = 1)) {
        stop(paste("Can't parse the cron part", cron_part))
      }
      values <- values[1]
    }
    if (values == "*") {
      return(do.call(seq, as.list(c(range, step))))
    }
    values <- parse_cron_range(values)
    return(values[seq(1, length(values), step)])
  }
}

#' cron_range
#' parse the range part of a cron string
#'
#' @param cron_range string range part of a cron string
#'
#' @return sequence of values determined by the cron range
#' @importFrom rlang %||%
#'
parse_cron_range <- function(cron_range) {
  if (grepl("-", cron_range)) {
    cron_range <- strsplit(cron_range, "-")[[1]]
  }

  cron_range <- sapply(cron_range, parse_cron_value)

  seq(cron_range[1], ifelse(is.na(cron_range[2]), cron_range[1], cron_range[2]))
}

parse_cron_value <- function(cron_value) {
  wday.abb <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
  if (tolower(cron_value) %in% tolower(month.abb)) {
    return(which(tolower(cron_value) == tolower(month.abb)))
  }

  if (tolower(cron_value) %in% tolower(wday.abb)) {
    return(which(tolower(cron_value) == tolower(wday.abb)))
  }

  ret <- suppressWarnings(as.integer(cron_value))
  if (!checkmate::test_count(ret)) {
    stop(paste("Can't parse the cron value", cron_value))
  }

  ret
}
