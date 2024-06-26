% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/var_group_nest.R
\name{var_group_nest}
\alias{var_group_nest}
\title{Nest a \code{var_grp_df} by the \code{z} columns}
\usage{
var_group_nest(var_grp_df, .subgroup = FALSE, .key = "data")
}
\arguments{
\item{var_grp_df}{the var_grp dataframe}

\item{.subgroup}{in the nested data frames also group the \code{y} columns}

\item{.key}{The name of the resulting nested column. Only applicable when
\code{...} isn't specified, i.e. in the case of \code{df \%>\% nest(.by = x)}.

If \code{NULL}, then \code{"data"} will be used by default.}
}
\value{
aa nested dataframe with \code{z}  columns and a \code{.key} column with
the \code{y} and \code{x} columns nested in it. The nested data will be grouped by
\code{y} columns.
}
\description{
Nest a \code{var_grp_df} by the \code{z} columns
}
\examples{
tmp = iris \%>\% dplyr::group_by(Species) \%>\% var_group(. ~ Petal.Width + Sepal.Width)
tmp2 = tmp \%>\% var_group_nest()
}
\concept{var_group}
