% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{interval_to_period}
\alias{interval_to_period}
\title{interval_to_period}
\usage{
interval_to_period(start_time, end_time)
}
\arguments{
\item{start_time}{POSIXct}

\item{end_time}{POSIXct}
}
\value{
\code{lubridate::Period} object for pretty printing of time differences
}
\description{
converts an interval of start and end times to a pretty printed elapsed time in the format
\verb{<hours> H <minutes> M <seconds> S}
}
