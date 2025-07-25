% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobs.R
\name{job_}
\alias{job_}
\alias{job_maybe_start}
\alias{job_start}
\alias{job_step}
\alias{job_on_error}
\alias{job_poll}
\alias{job_read}
\alias{job_finalize}
\alias{job_reset}
\title{job functions}
\usage{
job_maybe_start(flow_name, job_name)

job_start(flow_name, job_name)

job_step(flow_name, job_name)

job_on_error(flow_name, job_name, error)

job_poll(flow_name, job_name)

job_read(flow_name, job_name, timeout = 1)

job_finalize(flow_name, job_name)

job_reset(flow_name, job_name)
}
\arguments{
\item{flow_name}{string workflow name}

\item{job_name}{string job name}

\item{error}{error condition object}

\item{timeout}{milliseconds to wait for read before giving up, passed to \code{processx::process$poll_io}}
}
\value{
invisibly

invisibly

character vector of output from process. Names are one or more of \code{output}, \code{error} and \code{process} and match the names from \code{processx::poll_io}
}
\description{
run, poll, step, read, and finalize tessiflow jobs
}
\section{Functions}{
\itemize{
\item \code{job_maybe_start()}: check to see if it's time to start a job and call the job runner if it is.

the job will be started if following conditions are all true:
\itemize{
\item \code{runs-on} must match the computer name as listed in \code{Sys.info()["nodename"]}
\item \code{if} must be true in the current context
\item \code{needs} must have finished running since the last time this ran (with return value = 0 unless \code{if} evaluates to true)
\item the most recent run in \code{scheduled_runs} is after the last actual run time for this job
}

\item \code{job_start()}: run the job in a \code{callr} process

\item \code{job_step()}: Run the next step in a job

\item \code{job_on_error()}: Updates flows table and database, writes to log, and finalizes running session on error

\item \code{job_poll()}: poll the process \code{stdout} and \code{stderr} streams and call job_step when ready.

\item \code{job_read()}: Read \code{stdout} and \code{stderr} from the process and write to log. When ready, call job_step

\item \code{job_finalize()}: Closes R session, writes to log, console and database, and updates \code{retval}.

\item \code{job_reset()}: Resets job for next run, updates flows table but NOT database, writes to log

}}
