#' schedule_schtasks
#'
#' creates a job named `taskname` that runs every minute using Windows `schtasks.exe`
#'
#' @param expr expression to call in an Rscript process
#' @param taskname string name of task
#'
#' @return result of system call
schedule_schtasks <- function(expr, taskname = "tessiflow") {
  system2(Sys.which("schtasks"), c(
    "/create",
    "/tn", taskname,
    "/sc", "minute",
    "/mo", 1,
    "/tr", shQuote(script_expr(expr))
  ), stdout = TRUE)
}

#' @describeIn schedule_schtasks deletes a job with the name `taskname` using Windows `schtasks.exe`
unschedule_schtasks <- function(taskname = "tessiflow") {
  system2(Sys.which("schtasks"), c(
    "/delete", "/f",
    "/tn", taskname
  ), stdout = TRUE)
}

#' schedule_crontab
#'
#' creates a job named `taskname` that runs every minute using *nix `crontab`
#'
#' @param expr expression to call in an Rscript process
#' @param taskname string name of task
#'
#' @return result of system call
schedule_crontab <- function(expr, taskname = "tessiflow") {
  crontab_temp <- tempfile()
  crontab_updated <- c(
    system2("crontab", "-l", stdout = TRUE),
    paste("#", taskname),
    paste("* * * * *     ", script_expr(expr))
  )
  writeLines(crontab_updated, crontab_temp)
  system2(Sys.which("crontab"), crontab_temp)
}

#' @describeIn schedule_schtasks deletes a job named `taskname` using *nix `crontab`
unschedule_crontab <- function(taskname = "tessiflow") {
  crontab_temp <- tempfile()
  crontab_current <- system2("crontab", "-l", stdout = TRUE)
  rows <- grep(paste("#", taskname), crontab_current)
  writeLines(crontab_current[-c(rows, rows + 1)], crontab_temp)
  system2(Sys.which("crontab"), crontab_temp)
}

#' script_expr
#'
#' @param expr to call in an Rscript process
#'
#' @return Rscript command line call
script_expr <- function(expr) {
  expr <- rlang::enexpr(expr)
  if (!is.call(expr)) expr <- eval(expr, envir = parent.frame())

  r_command <- paste(deparse(expr, width.cutoff = 500), collapse = ";")

  paste(
    paste0(
      file.path(Sys.getenv("R_HOME"), "bin", "Rscript"),
      ifelse(.Platform$OS.type == "windows", ".exe", "")
    ),
    "-e", shQuote(r_command),
    "--no-save", "--no-restore"
  )
}
