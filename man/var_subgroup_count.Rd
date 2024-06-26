% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/var_group_count.R
\name{var_subgroup_count}
\alias{var_subgroup_count}
\title{The number of major and sub groups (\code{z} and \code{y} categories) in a \code{var_grp_df}}
\usage{
var_subgroup_count(var_grp_df, .stratified = FALSE)
}
\arguments{
\item{var_grp_df}{the var_grp dataframe}

\item{.stratified}{if \code{TRUE} return the subgroup count stratified by major groups as a dataframe}
}
\value{
a count of groups and subgroups
}
\description{
The number of major and sub groups (\code{z} and \code{y} categories) in a \code{var_grp_df}
}
\examples{
tmp = iris \%>\% dplyr::group_by(Species) \%>\% var_group(. ~ Petal.Width + Sepal.Width)
tmp \%>\% var_subgroup_count()
}
\concept{var_group}
