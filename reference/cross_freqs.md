# Run crosstabs in R

Create a frequencies table with multiple distinct grouping
variables/banners

## Usage

``` r
cross_freqs(
  dataset,
  group_vars,
  ...,
  stat = c("percent", "mean", "median", "min", "max", "quantile", "summary"),
  percentile = NULL,
  nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  wide = FALSE,
  exclude_groups = FALSE,
  include_overall = FALSE
)
```

## Arguments

- dataset:

  A dataframe.

- group_vars:

  Accepts a character vector of variable names. The variables by which
  you want to subset your freqeuncies. In a traditional crosstab, these
  would be the banner variables.

- ...:

  The unquoted names of a set of variables in the dataset. If nothing is
  specified, the function runs a frequency on every column in given
  dataset.

- stat:

  Character, stat to run. Currently accepts 'percent,' 'mean,' 'median,'
  'min,' 'max,' 'quantile,' and 'summary' (default: 'percent').

- percentile:

  Double, for use when stat = 'quantile.' Input should be a real number
  x such that 0 \<= x \<= 100. Stands for percentile rank, which is a
  quantile relative to a 100-point scale. (default:NULL)

- nas:

  Boolean, whether or not to include NAs in the tabulation (default:
  TRUE).

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

- wide:

  Boolean, whether the dataframe should be one long dataframe (FALSE) or
  a wide and nested dataframe, nested on the group_vars (TRUE) (default:
  FALSE)

- exclude_groups:

  Boolean, argument only applies if group_vars are also included as
  freqs vars - group_vars are included as freqs vars if using select()
  to run cross_freqs on all variables in the dataset. FALSE will INclude
  group_vars as freqs vars. TRUE will EXclude group_vars from also being
  freqs vars (default: FALSE)

- include_overall:

  Boolean, whether to include the overall frequency levels for variables
  (default = FALSE)

## Value

A dataframe with the variable names, prompts, values, labels, counts,
stats, and resulting calculations, split out by subgroups (group_vars).

## Examples

``` r
GROUP_VARS <-
  mtcars %>%
  dplyr::select(
    am,
    vs
  ) %>%
  names()

GROUP_VARS <- c("am", "vs")

mtcars %>% cross_freqs(
  group_vars = GROUP_VARS,
  gear,
  carb
)
#> Adding missing grouping variables: `am`
#> Adding missing grouping variables: `am`
#> Adding missing grouping variables: `vs`
#> Adding missing grouping variables: `vs`
#> # A tibble: 27 × 8
#>    group_var_name group_var variable value label     n stat    result
#>    <chr>          <fct>     <chr>    <chr> <chr> <int> <chr>    <dbl>
#>  1 am             0         gear     3     3        15 percent   0.79
#>  2 am             0         gear     4     4         4 percent   0.21
#>  3 am             1         gear     4     4         8 percent   0.62
#>  4 am             1         gear     5     5         5 percent   0.38
#>  5 am             0         carb     1     1         3 percent   0.16
#>  6 am             0         carb     2     2         6 percent   0.32
#>  7 am             0         carb     3     3         3 percent   0.16
#>  8 am             0         carb     4     4         7 percent   0.37
#>  9 am             1         carb     1     1         4 percent   0.31
#> 10 am             1         carb     2     2         4 percent   0.31
#> # ℹ 17 more rows
```
