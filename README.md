matchmaker R package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# matchmaker

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/matchmaker)](https://CRAN.R-project.org/package=matchmaker)
[![Travis build
status](https://travis-ci.org/reconhub/matchmaker.svg?branch=master)](https://travis-ci.org/reconhub/matchmaker)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/reconhub/matchmaker?branch=master&svg=true)](https://ci.appveyor.com/project/reconhub/matchmaker)
<!-- badges: end -->

The goal of matchmaker is to provide dictionary-based cleaning in a
flexible manner.

## Installation

You can install the development version of matchmaker from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("reconhub/matchmaker")
```

## Example

The matchmaker package has two user-facing functions that perform
dictionary-based cleaning:

  - `match_vec()` will translate the values in a single vector
  - `match_df()` will translate values in all specified columns of a
    data frame

<!-- end list -->

``` r
library(matchmaker)
dict     <- read.csv(matchmaker_example("spelling-dictionary.csv"), 
                     stringsAsFactors = FALSE)
dat      <- read.csv(matchmaker_example("coded-data.csv"), 
                     stringsAsFactors = FALSE)
dat$date <- as.Date(dat$date)
#
# Clean spelling based on dictionary ----------------------------- 
#
dict # show the dict
#>     options       values                 grp orders
#> 1         y          Yes         readmission      1
#> 2         n           No         readmission      2
#> 3         u      Unknown         readmission      3
#> 4  .missing      Missing         readmission      4
#> 5         0          Yes             treated      1
#> 6         1           No             treated      2
#> 7  .missing      Missing             treated      3
#> 8         1  Facility  1            facility      1
#> 9         2  Facility  2            facility      2
#> 10        3  Facility  3            facility      3
#> 11        4  Facility  4            facility      4
#> 12        5  Facility  5            facility      5
#> 13        6  Facility  6            facility      6
#> 14        7  Facility  7            facility      7
#> 15        8  Facility  8            facility      8
#> 16        9  Facility  9            facility      9
#> 17       10  Facility 10            facility     10
#> 18 .default      Unknown            facility     11
#> 19        0          0-9           age_group      1
#> 20       10        10-19           age_group      2
#> 21       20        20-29           age_group      3
#> 22       30        30-39           age_group      4
#> 23       40        40-49           age_group      5
#> 24       50          50+           age_group      6
#> 25     high         High .regex ^lab_result_      1
#> 26     norm       Normal .regex ^lab_result_      2
#> 27      inc Inconclusive .regex ^lab_result_      3
#> 28        y          yes             .global    Inf
#> 29        n           no             .global    Inf
#> 30        u      unknown             .global    Inf
#> 31      unk      unknown             .global    Inf
#> 32      oui          yes             .global    Inf
#> 33 .missing      missing             .global    Inf
head(dat) # show the data
#>       id       date readmission treated facility age_group lab_result_01 lab_result_02 lab_result_03 has_symptoms followup
#> 1 ef267c 2019-07-08        <NA>       0        C        10           unk          high           inc         <NA>        u
#> 2 e80a37 2019-07-07           y       0        3        10           inc           unk          norm            y      oui
#> 3 b72883 2019-07-07           y       1        8        30           inc          norm           inc                   oui
#> 4 c9ee86 2019-07-09           n       1        4        40           inc           inc           unk            y      oui
#> 5 40bc7a 2019-07-12           n       1        6         0          norm           unk          norm         <NA>        n
#> 6 46566e 2019-07-14           y      NA        B        50           unk           unk           inc         <NA>     <NA>

res1 <- match_df(dat,
                 dictionary = dict,
                 from = "options",
                 to = "values",
                 by = "grp")
head(res1)
#>       id       date readmission treated    facility age_group lab_result_01 lab_result_02 lab_result_03 has_symptoms followup
#> 1 ef267c 2019-07-08     Missing     Yes     Unknown     10-19       unknown          High  Inconclusive      missing  unknown
#> 2 e80a37 2019-07-07         Yes     Yes Facility  3     10-19  Inconclusive       unknown        Normal          yes      yes
#> 3 b72883 2019-07-07         Yes      No Facility  8     30-39  Inconclusive        Normal  Inconclusive      missing      yes
#> 4 c9ee86 2019-07-09          No      No Facility  4     40-49  Inconclusive  Inconclusive       unknown          yes      yes
#> 5 40bc7a 2019-07-12          No      No Facility  6       0-9        Normal       unknown        Normal      missing       no
#> 6 46566e 2019-07-14         Yes Missing     Unknown       50+       unknown       unknown  Inconclusive      missing  missing
```
