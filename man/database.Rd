% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flows_log.R, R/performance_log.R
\name{database}
\alias{database}
\alias{flows_log_open}
\alias{flows_log_close}
\alias{flows_log_create}
\alias{performance_log_open}
\alias{performance_log_create}
\alias{performance_log_close}
\title{Tessiflow database utilities}
\usage{
flows_log_open(flows_log_dir = config::get("tessiflow.log"))

flows_log_close()

flows_log_create()

performance_log_open(flows_log_dir = config::get("tessiflow.log"))

performance_log_create()

performance_log_close()
}
\arguments{
\item{flows_log_dir}{directory where the SQLite database is stored}
}
\value{
invisible
}
\description{
database
}
\section{Functions}{
\itemize{
\item \code{flows_log_open()}: opens the jobs database connection

\item \code{flows_log_close()}: closes the jobs database connection

\item \code{flows_log_create()}: Create the SQLite log table

\item \code{performance_log_open()}: opens the performance database connection

\item \code{performance_log_create()}: Create the SQLite performance log table

\item \code{performance_log_close()}: closes the performance database connection

}}
