# pkgutils

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/679201735.svg)](https://zenodo.org/badge/latestdoi/679201735)
[![pkgutils status
badge](https://bristol-vaccine-centre.r-universe.dev/badges/pkgutils)](https://bristol-vaccine-centre.r-universe.dev)
[![R-CMD-check](https://github.com/bristol-vaccine-centre/pkgutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bristol-vaccine-centre/pkgutils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`pkgutils` is a set of functions for making writing R functions easier. The 
intention is to be included as a dependency in R packages and therefore is 
designed with the package author as the end user. The functions provided fall
into 2 categories, those that focus on validating parameters for completeness
and consistency of length, and those that focus on working with dataframe inputs
which have multiple layers of grouping, or where a function may need to be 
configured to operate on different columns, or different column groups.

It is a maturing package and behaviour may evolve over time.

## Installation

`pkgutils` is part of the [Bristol Vaccine Centre r-universe](https://bristol-vaccine-centre.r-universe.dev/).
Installation from there is as follows:

``` r
options(repos = c(
  "bristol-vaccine-centre" = 'https://bristol-vaccine-centre.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))

# Download and install growthrates in R
install.packages("pkgutils")
```

You can install the development version of `pkgutils` from 
[GitHub](https://github.com/bristol-vaccine-centre/pkgutils) with:

``` r
# install.packages("devtools")
devtools::install_github("bristol-vaccine-centre/pkgutils")
```