% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schtasks_cron.R
\name{schedule_crontab}
\alias{schedule_crontab}
\title{schedule_crontab}
\usage{
schedule_crontab(command, taskname = "tessiflow")
}
\arguments{
\item{command}{string command line expression}

\item{taskname}{string name of task}
}
\value{
result of system call
}
\description{
creates a job named \code{taskname} that runs every minute using *nix \code{crontab}
}
