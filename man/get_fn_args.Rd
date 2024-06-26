% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fn_args.R
\name{get_fn_args}
\alias{get_fn_args}
\title{Fully evaluate the arguments from a function call as a named list.}
\usage{
get_fn_args(env = rlang::caller_env(), missing = TRUE)
}
\arguments{
\item{env}{the environment to check (default \code{rlang::caller_env()})}

\item{missing}{include missing parameters in list (default \code{TRUE})?}
}
\value{
a named list of the arguments of the enclosing function
}
\description{
Used within a function this provides access to the actual arguments provided
during invocation of the parent function, plus any default values. The
parameters are evaluated eagerly before being returned (so symbols and expressions
must resolve to real values.)
}
\examples{

ftest = function(a,b,c="default",...) {
  tmp = get_fn_args()
  tmp
}

ftest(a=1, b=2)

# missing param `b` - empty values are returned just as a name in the environment
# with no value but which can be checked for as if in the environment.
tmp = ftest(a=1)
class(tmp$b)
rlang::is_missing(tmp$b)
b = 1
rlang::is_missing(tmp$b)

# extra param `d` and default parameter `c`
ftest(a=1, b=2, d="another")

# does not work
try(ftest(a=1, b=2, d=another))
# does work
tmp = ftest( a=1, d= as.symbol("another") )
# also does work
another =5
ftest( a=1, d= another)

# Filter out missing values

ftest2 = function(a,b,c="default",...) {
  tmp = get_fn_args(missing=FALSE)
  tmp
}

ftest2(a=1)

}
