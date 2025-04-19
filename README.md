
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EGM

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/EGM)](https://CRAN.R-project.org/package=EGM)
[![](http://cranlogs.r-pkg.org/badges/grand-total/EGM?color=blue)](https://cran.r-project.org/package=EGM)
[![R-CMD-check](https://github.com/shah-in-boots/EGM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shah-in-boots/EGM/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/shah-in-boots/egm/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/shah-in-boots/egm/actions/workflows/pkgdown.yaml)
[![test-coverage](https://github.com/shah-in-boots/EGM/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/shah-in-boots/EGM/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/shah-in-boots/egm/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shah-in-boots/egm?branch=main)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/shah-in-boots/EGM)](https://github.com/shah-in-boots/EGM/graphs/commit-activity)
[![DOI](https://zenodo.org/badge/405145024.svg)](https://doi.org/10.5281/zenodo.15244228)
<!-- badges: end -->

The purpose of `{EGM}` is to work with electrophysiology (EP) signal
data to help understand and simplify the complexity of complex,
multi-channel electrical signal data. The target audience is those
working, in particular, with *cardiac electrophysiology data*, from
intracardiac electrograms to surface electrocardiography. The package is
heavily inspired by and gains additional functionality from the
[Waveform Database (WFDB) software
package](https://physionet.org/content/wfdb/10.7.0/).

The goals and major arms of this software are…

1.  Adapt `WFDB`-compatible signal data to an `R` format that is
    interchangeable
2.  Provide simple visualization tools to work with short elements of
    raw signal data
3.  Allow for annotation of intracardiac electrograms in an interactive
    manner
4.  Train and allow development of learning algorithms for the
    evaluation of multi-channel time series data

Please see the vignettes for further details on usage.

## Installation

You can install the released version of `{EGM}` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("EGM")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shah-in-boots/EGM")
```

Notably, the package software utilizes the WFDB software if it is
available. You can expect errors in calling commands until the software
is appropriately installed. Further installation instructions are
available on the [Github site](https://github.com/bemoody/wfdb).
