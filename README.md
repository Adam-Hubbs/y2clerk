
<!-- README.md is generated from README.Rmd. Please edit that file -->

# y2clerk

<!-- badges: start -->

<!-- badges: end -->

## Overview

y2clerk exists to quickly create formatted frequencies tables. It
leverages the tidyverse, allowing the user to `%>%` in data frames,
`select()` down to lists of variables, `group_by()` others, and include
weights for the frequencies. There are two main functions in y2clerk:

  - `freqs()` - creates a frequency table including both ns and
    percentages for each level of a variable or list of variables.
    `freqs()` is flexible and can also be used for calculating means,
    medians, or other quartiles while including weighting variables.
  - `verbatims_y2()` - creates a table with similar formatting to freqs.
    However, this function is used for string variables and creates a
    table with all responses given.
  - `cross_freqs()` - creates a frequency table similar to cross tabs.
    Each group\_var given in the function acts as its own unique banner
    for every variable listed in the freqs. Whereas *group\_by %\>%
    freqs* produces a set of frequencies grouped by a single variable,
    *cross\_freqs* is designed to produce a set of frequencies grouped
    by multiple different grouping variables one after another and then
    combines these results into a single
dataframe.

## Installation

<!-- You can install the released version of y2clerk from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("y2clerk") -->

<!-- ``` -->

You can install the most updated package version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("y2analytics/y2clerk")
```

## Examples

Below you will find a few basic examples which show you how to quickly
get a frequencies table with `freqs()`:

``` r
library(y2clerk)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
df <- data.frame(
  a = c(1, 2, 2, 3, 4, 2, NA),
  b = c(1, 2, 2, 3, 4, 1, NA),
  weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
)

freqs(df, a, b)
#>    variable value label n    stat result
#> 1         a     1     1 1 percent   0.14
#> 2         a     2     2 3 percent   0.43
#> 3         a     3     3 1 percent   0.14
#> 4         a     4     4 1 percent   0.14
#> 5         a  <NA>  <NA> 1 percent   0.14
#> 6         b     1     1 2 percent   0.29
#> 7         b     2     2 2 percent   0.29
#> 8         b     3     3 1 percent   0.14
#> 9         b     4     4 1 percent   0.14
#> 10        b  <NA>  <NA> 1 percent   0.14
df %>% freqs(a, b, wt = weights)
#>    variable value label   n    stat result
#> 1         a     1     1 0.9 percent   0.13
#> 2         a     2     2 3.0 percent   0.43
#> 3         a     3     3 1.1 percent   0.16
#> 4         a     4     4 1.0 percent   0.14
#> 5         a  <NA>  <NA> 1.0 percent   0.14
#> 6         b     1     1 1.9 percent   0.27
#> 7         b     2     2 2.0 percent   0.29
#> 8         b     3     3 1.1 percent   0.16
#> 9         b     4     4 1.0 percent   0.14
#> 10        b  <NA>  <NA> 1.0 percent   0.14
freqs(df, stat = 'mean', nas = FALSE)
#> # A tibble: 3 x 6
#>   variable value label     n stat  result
#>   <chr>    <chr> <chr> <dbl> <chr>  <dbl>
#> 1 a        ""    ""        6 mean    2.33
#> 2 b        ""    ""        6 mean    2.17
#> 3 weights  ""    ""        7 mean    1
freqs(df, stat = 'mean', nas = FALSE, wt = weights)
#> # A tibble: 2 x 6
#>   variable value label     n stat            result
#>   <chr>    <chr> <chr> <dbl> <chr>            <dbl>
#> 1 a        ""    ""        6 mean - weighted   2.37
#> 2 b        ""    ""        6 mean - weighted   2.2
df %>% group_by(a) %>% freqs(b, stat = 'mean', nas = FALSE, wt = weights)
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> # A tibble: 4 x 7
#>   group_var variable value label     n stat            result
#>       <dbl> <chr>    <chr> <chr> <dbl> <chr>            <dbl>
#> 1         1 b        ""    ""      0.9 mean - weighted   1   
#> 2         2 b        ""    ""      3   mean - weighted   1.67
#> 3         3 b        ""    ""      1.1 mean - weighted   3   
#> 4         4 b        ""    ""      1   mean - weighted   4
```

## Help

If you have issues using y2clerk, please post your issue on
[GitHub](https://github.com/y2analytics/y2clerk/issues) along with a
minimal reproducible example. We will do our best to address your issues
and get them fixed for the next version of y2clerk.
