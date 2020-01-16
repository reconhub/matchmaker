matchmaker R package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/matchmaker)](https://CRAN.R-project.org/package=matchmaker)
[![Travis build
status](https://travis-ci.org/reconhub/matchmaker.svg?branch=master)](https://travis-ci.org/reconhub/matchmaker)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/reconhub/matchmaker?branch=master&svg=true)](https://ci.appveyor.com/project/reconhub/matchmaker)
[![Codecov test
coverage](https://codecov.io/gh/reconhub/matchmaker/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/matchmaker?branch=master)
<!-- badges: end -->

The goal of {matchmaker} is to provide dictionary-based cleaning for R
users in a simple and intuitive manner built on the
[{forcats}](https://forcats.tidyverse.org) package. Some of the features
of this package include:

  - preservation of factor orders
  - ability to specify explicit and implicit missing values
  - option to replace by fuzzy matching (regular expressions, anchored
    by default)
  - optional variable selection by fuzzy matching

## Installation

You can install {matchmaker} from CRAN:

``` r
install.packages("matchmaker")
```

## Example

The matchmaker package has two user-facing functions that perform
dictionary-based cleaning:

  - `match_vec()` will translate the values in a single vector
  - `match_df()` will translate values in all specified columns of a
    data frame

Each of these functions have four manditory options:

  - `x`: your data. This will be a vector or data frame depending on the
    function.
  - `dictionary`: This is a data frame with at least two columns
    specifying keys and values to modify
  - `from`: a character or number specifying which column contains the
    keys
  - `to`: a character or number specifying which column contains the
    values

Mostly, users will be working with `match_df()` to transform values
across specific columns. A typical workflow would be to:

1.  construct your dictionary in a spreadsheet program based on your
    data
2.  read in your data and dictionary to data frames in R
3.  match\!

<!-- end list -->

``` r
library("matchmaker")

# Read in data set
dat <- read.csv(matchmaker_example("coded-data.csv"),
  stringsAsFactors = FALSE
)
dat$date <- as.Date(dat$date)

# Read in dictionary
dict <- read.csv(matchmaker_example("spelling-dictionary.csv"),
  stringsAsFactors = FALSE
)
```

### Data

This is the top of our data set, generated for example
purposes

| id     | date       | readmission | treated | facility | age\_group | lab\_result\_01 | lab\_result\_02 | lab\_result\_03 | has\_symptoms | followup |
| :----- | :--------- | :---------- | ------: | :------- | ---------: | :-------------- | :-------------- | :-------------- | :------------ | :------- |
| ef267c | 2019-07-08 | NA          |       0 | C        |         10 | unk             | high            | inc             | NA            | u        |
| e80a37 | 2019-07-07 | y           |       0 | 3        |         10 | inc             | unk             | norm            | y             | oui      |
| b72883 | 2019-07-07 | y           |       1 | 8        |         30 | inc             | norm            | inc             |               | oui      |
| c9ee86 | 2019-07-09 | n           |       1 | 4        |         40 | inc             | inc             | unk             | y             | oui      |
| 40bc7a | 2019-07-12 | n           |       1 | 6        |          0 | norm            | unk             | norm            | NA            | n        |
| 46566e | 2019-07-14 | y           |      NA | B        |         50 | unk             | unk             | inc             | NA            | NA       |

### Dictionary

The dictionary looks like this:

| options  | values       | grp                   | orders |
| :------- | :----------- | :-------------------- | -----: |
| y        | Yes          | readmission           |      1 |
| n        | No           | readmission           |      2 |
| u        | Unknown      | readmission           |      3 |
| .missing | Missing      | readmission           |      4 |
| 0        | Yes          | treated               |      1 |
| 1        | No           | treated               |      2 |
| .missing | Missing      | treated               |      3 |
| 1        | Facility 1   | facility              |      1 |
| 2        | Facility 2   | facility              |      2 |
| 3        | Facility 3   | facility              |      3 |
| 4        | Facility 4   | facility              |      4 |
| 5        | Facility 5   | facility              |      5 |
| 6        | Facility 6   | facility              |      6 |
| 7        | Facility 7   | facility              |      7 |
| 8        | Facility 8   | facility              |      8 |
| 9        | Facility 9   | facility              |      9 |
| 10       | Facility 10  | facility              |     10 |
| .default | Unknown      | facility              |     11 |
| 0        | 0-9          | age\_group            |      1 |
| 10       | 10-19        | age\_group            |      2 |
| 20       | 20-29        | age\_group            |      3 |
| 30       | 30-39        | age\_group            |      4 |
| 40       | 40-49        | age\_group            |      5 |
| 50       | 50+          | age\_group            |      6 |
| high     | High         | .regex ^lab\_result\_ |      1 |
| norm     | Normal       | .regex ^lab\_result\_ |      2 |
| inc      | Inconclusive | .regex ^lab\_result\_ |      3 |
| y        | yes          | .global               |    Inf |
| n        | no           | .global               |    Inf |
| u        | unknown      | .global               |    Inf |
| unk      | unknown      | .global               |    Inf |
| oui      | yes          | .global               |    Inf |
| .missing | missing      | .global               |    Inf |

### Matching

``` r
# Clean spelling based on dictionary -----------------------------
cleaned <- match_df(dat,
  dictionary = dict,
  from = "options",
  to = "values",
  by = "grp"
)
head(cleaned)
#>       id       date readmission treated    facility age_group
#> 1 ef267c 2019-07-08     Missing     Yes     Unknown     10-19
#> 2 e80a37 2019-07-07         Yes     Yes Facility  3     10-19
#> 3 b72883 2019-07-07         Yes      No Facility  8     30-39
#> 4 c9ee86 2019-07-09          No      No Facility  4     40-49
#> 5 40bc7a 2019-07-12          No      No Facility  6       0-9
#> 6 46566e 2019-07-14         Yes Missing     Unknown       50+
#>   lab_result_01 lab_result_02 lab_result_03 has_symptoms followup
#> 1       unknown          High  Inconclusive      missing  unknown
#> 2  Inconclusive       unknown        Normal          yes      yes
#> 3  Inconclusive        Normal  Inconclusive      missing      yes
#> 4  Inconclusive  Inconclusive       unknown          yes      yes
#> 5        Normal       unknown        Normal      missing       no
#> 6       unknown       unknown  Inconclusive      missing  missing
```
