# Run frequencies for multiple select variables

Filters out rows that are completely NULL values (if respondent did not
answer question) then runs freqs

## Usage

``` r
multi_freqs(
  dataset,
  ...,
  remove_nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  unweighted_ns = FALSE,
  show_missing_levels = TRUE
)
```

## Arguments

- dataset:

  A dataframe.

- ...:

  The unquoted names of a set of variables in the dataset referring to
  variable "stems". If nothing is specified, the function runs a
  frequency on every column in given dataset.

- remove_nas:

  Boolean, after freqs is run (which always includes NAs), whether or
  not to filter out counts of NA value (default: TRUE).

- wt:

  The unquoted name of a weighting variable in the dataset (default:
  NULL).

- prompt:

  Boolean, whether or not to include the prompt in the dataset (default:
  FALSE).

- digits:

  Integer, number of significant digits for rounding (default: 2).

- nas_group:

  Boolean, whether or not to include NA values for the grouping variable
  in the tabulation (default: TRUE).

- factor_group:

  Boolean, whether or not to convert the grouping variable to a factor
  and use its labels instead of its underlying numeric values (default:
  FALSE)

- unweighted_ns:

  Boolean, whether the 'n' column in the freqs table should be
  UNweighted while results ARE weighted. This argument can only be used
  if a wt variable is used. If no weight variable is used, the 'n'
  column will always be unweighted (default: FALSE).

- show_missing_levels:

  Boolean, whether to keep response levels with no data (default: TRUE)

## Value

A dataframe with the variable names, prompts, values, labels, counts,
stats, and resulting calculations.

## Examples

``` r
df <- data.frame(
  a = c(1, 2, 3, 1, 2, 3, 1),
  Q1_1 = c(1, NA, 1, 1, NA, 1, NA),
  Q1_2 = c(1, 1, NA, 1, NA, 1, NA),
  Q1_3 = c(NA, 1, 1, NA, 4, 1, NA),
  weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
) |>
  tidyr::as_tibble()


# All 3 methods below give the same output
multi_freqs(df, Q1_1)
#> Variable stem "Q1" successfully freq'd
#> # A frequency tibble: 4 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 Q1_1     1     1         4 percent   0.67
#> 2 Q1_2     1     1         4 percent   0.67
#> 3 Q1_3     1     1         3 percent   0.5 
#> 4 Q1_3     4     4         1 percent   0.17
df |> multi_freqs(Q1_1)
#> Variable stem "Q1" successfully freq'd
#> # A frequency tibble: 4 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 Q1_1     1     1         4 percent   0.67
#> 2 Q1_2     1     1         4 percent   0.67
#> 3 Q1_3     1     1         3 percent   0.5 
#> 4 Q1_3     4     4         1 percent   0.17
df |>
  dplyr::select(dplyr::starts_with("Q1")) |>
  multi_freqs()
#> Variable stem "Q1" successfully freq'd
#> # A frequency tibble: 4 × 6
#>   variable value label     n stat    result
#>   <chr>    <chr> <chr> <int> <chr>    <dbl>
#> 1 Q1_1     1     1         4 percent   0.67
#> 2 Q1_2     1     1         4 percent   0.67
#> 3 Q1_3     1     1         3 percent   0.5 
#> 4 Q1_3     4     4         1 percent   0.17


# Grouped examples with weights (both have same outputs)
df |>
  dplyr::group_by(a) |>
  multi_freqs(Q1_1, wt = weights)
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 8 × 7
#> # Groups:   group_var [3]
#>   group_var variable value label     n stat    result
#>       <dbl> <chr>    <chr> <chr> <dbl> <chr>    <dbl>
#> 1         1 Q1_1     1     1       2   percent   1   
#> 2         3 Q1_1     1     1       2.1 percent   1   
#> 3         1 Q1_2     1     1       2   percent   1   
#> 4         2 Q1_2     1     1       0.9 percent   0.47
#> 5         3 Q1_2     1     1       1   percent   0.48
#> 6         2 Q1_3     1     1       0.9 percent   0.47
#> 7         2 Q1_3     4     4       1   percent   0.53
#> 8         3 Q1_3     1     1       2.1 percent   1   
df |>
  dplyr::group_by(a) |>
  dplyr::select(starts_with("Q1"), weights) |>
  multi_freqs(wt = weights)
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> Adding missing grouping variables: `a`
#> Variable stem "Q1" successfully freq'd
#> # A tibble: 8 × 7
#> # Groups:   group_var [3]
#>   group_var variable value label     n stat    result
#>       <dbl> <chr>    <chr> <chr> <dbl> <chr>    <dbl>
#> 1         1 Q1_1     1     1       2   percent   1   
#> 2         3 Q1_1     1     1       2.1 percent   1   
#> 3         1 Q1_2     1     1       2   percent   1   
#> 4         2 Q1_2     1     1       0.9 percent   0.47
#> 5         3 Q1_2     1     1       1   percent   0.48
#> 6         2 Q1_3     1     1       0.9 percent   0.47
#> 7         2 Q1_3     4     4       1   percent   0.53
#> 8         3 Q1_3     1     1       2.1 percent   1   
```
