
<!-- README.md is generated from README.Rmd. Please edit that file -->

# egm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/egm)](https://CRAN.R-project.org/package=egm)
[![R-CMD-check](https://github.com/shah-in-boots/egm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shah-in-boots/egm/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/shah-in-boots/egm/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/shah-in-boots/egm/actions/workflows/test-coverage.yaml)
[![pkgdown](https://github.com/shah-in-boots/egm/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/shah-in-boots/egm/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

When looking at cardiac electrogram data, an important practice is
identifying intervals between depolarizations. These range from
*sinoatrial* conduction properties, *atrioventricular* and
*ventriculoatrial* intervals, *Hissian* intervals, etc. These names
(STIMULATION, HISSIAN, INTERVALS, VENTRICULAR, ATRIAL) were put into an
anagram for the name of the package: `egm`.

The goal of `egm` is to work with electrophysiology (EP) signal data to
help evaluate intervals, pacing maneuvers, stimulation protocols, as
well as generate informative plots for teaching/learning and
understanding of EP studies.

## Installation

You can install the released version of egm from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("egm")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shah-in-boots/egm")
```
