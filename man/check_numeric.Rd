% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/params_check_numeric.R
\name{check_numeric}
\alias{check_numeric}
\title{Checks a set of variables can be coerced to numeric and coerces them}
\usage{
check_numeric(
  ...,
  .message = "`{param}` is non-numeric ({err}).",
  .env = rlang::caller_env()
)
}
\arguments{
\item{...}{a list of symbols}

\item{.message}{a glue spec containing \code{{param}} as the name of the parameter and \code{{err}} the cause fo the error}

\item{.env}{the environment to check (defaults to calling environment)}
}
\value{
nothing. called for side effects. throws error if not all variables can be coerced.
}
\description{
N.B. This only works for the specific environment (to prevent weird side effects)
}
\examples{
a = c(1:4L)
b = c("1",NA,"3.3")
f = NULL
g = NA
check_numeric(a,b,f,g)

c = c("dfsfs")
try(check_numeric(c,d, mean))
}
\concept{params_check}
