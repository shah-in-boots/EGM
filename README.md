
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shiva

<!-- badges: start -->

[![R-CMD-check](https://github.com/asshah4/shiva/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/shiva/actions)
[![Codecov test
coverage](https://codecov.io/gh/asshah4/shiva/branch/main/graph/badge.svg)](https://codecov.io/gh/asshah4/shiva?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

When looking at cardiac electrogram data, an important practice is
identifying intervals between depolarizations. These range from
*sinoatrial* conduction properties, *atrioventricular* and
*ventriculoatrial* intervals, *Hissian* intervals, etc. These names
(STIMULATION, HISSIAN, INTERVALS, VENTRICULAR, ATRIAL) were put into an
anagram for the name of the package: `shiva`.

The goal of `shiva` is to work with electrophysiology (EP) signal data
to help evaluate intervals, pacing maneuvers, stimulation protocols, as
well as generate informative plots for teaching/learning and
understanding of EP studies.

## Installation

You can install the released version of shiva from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("shiva")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/shiva")
```
