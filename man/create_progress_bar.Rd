% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progress_utils.R
\name{create_progress_bar}
\alias{create_progress_bar}
\title{Progress Tracking Utilities for LPPLS Analysis}
\usage{
create_progress_bar(
  total,
  format = "[:bar] :percent (:current/:total) ETA: :eta Elapsed: :elapsed"
)
}
\arguments{
\item{total}{Total number of iterations}

\item{format}{Progress bar format}
}
\value{
Progress bar object or simple counter
}
\description{
Functions for tracking progress and benchmarking LPPLS computations
}
\keyword{Bar}
\keyword{Create}
\keyword{Progress}
\keyword{internal}
