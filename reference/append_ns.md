# Tastefully combine "label" and "n" columns

Uses the output from the y2clerk freqs() function and adds (n = ...) to
the end of each label, based off the column "n". Useful for charts that
require ns for all levels

## Usage

``` r
append_ns(dataset, append_to = label, by_group_var = FALSE, newline = FALSE)
```

## Arguments

- dataset:

  A dataframe

- append_to:

  (default: label). The variable to which you wish to add (n = ...)

- by_group_var:

  (default: FALSE). If FALSE, takes ns by row. If TRUE, takes group ns
  by group_var levels. Set to TRUE if making a stacked bar chart

- newline:

  (default: FALSE). If FALSE, (n = ...) comes after a space " ". If
  TRUE, it comes after a hard return

## Examples

``` r
# by_group_var = FALSE
ToothGrowth |>
  freqs(supp) |>
  append_ns()
#> # A frequency tibble: 2 × 6
#>   variable value label           n stat    result
#>   <chr>    <chr> <chr>       <int> <chr>    <dbl>
#> 1 supp     1     OJ (n = 30)    30 percent    0.5
#> 2 supp     2     VC (n = 30)    30 percent    0.5

ToothGrowth |>
  dplyr::group_by(supp) |>
  freqs(dose) |>
  append_ns(newline = TRUE)
#> Adding missing grouping variables: `supp`
#> # A tibble: 6 × 7
#> # Groups:   group_var [2]
#>   group_var variable value label               n stat    result
#>   <fct>     <chr>    <chr> <chr>           <int> <chr>    <dbl>
#> 1 OJ        dose     0.5   "0.5\n(n = 10)"    10 percent   0.33
#> 2 OJ        dose     1     "1\n(n = 10)"      10 percent   0.33
#> 3 OJ        dose     2     "2\n(n = 10)"      10 percent   0.33
#> 4 VC        dose     0.5   "0.5\n(n = 10)"    10 percent   0.33
#> 5 VC        dose     1     "1\n(n = 10)"      10 percent   0.33
#> 6 VC        dose     2     "2\n(n = 10)"      10 percent   0.33

# by_group_var = TRUE
ToothGrowth |>
  dplyr::group_by(supp) |>
  freqs(dose) |>
  append_ns(
    append_to = group_var,
    by_group_var = TRUE
  )
#> Adding missing grouping variables: `supp`
#> # A tibble: 6 × 7
#> # Groups:   group_var [2]
#>   group_var   variable value label     n stat    result
#>   <chr>       <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 OJ (n = 30) dose     0.5   0.5      10 percent   0.33
#> 2 OJ (n = 30) dose     1     1        10 percent   0.33
#> 3 OJ (n = 30) dose     2     2        10 percent   0.33
#> 4 VC (n = 30) dose     0.5   0.5      10 percent   0.33
#> 5 VC (n = 30) dose     1     1        10 percent   0.33
#> 6 VC (n = 30) dose     2     2        10 percent   0.33
```
