# Create a dataframe of open-ended questions formatted for internbot, used to create appendices

Formats open-ended questions with the proper columns for internbot:
variable, prompt, label, base_ns

## Usage

``` r
verbatims_y2(dataset, ...)
```

## Arguments

- dataset:

  no default. Usually piped in from your main dataset

- ...:

  The names of the openended variables from your dataset you want to
  include in your new dataframe

## Examples

``` r
responses <- tibble::tibble(
  var1 = c(
    'I like to talk about dogs',
    'Dogs are cool but cats are aight too',
    'I prefer dogs over cats',
    "My dog's collars are always too tight",
    'One last sentence about dogs',
    'Cats collars are typically cooler than dogs'
  )
)

verbatims_y2(responses, var1)
#> Warning: You are working with variables that have no labeling. You may want to consider adding a prompt before continuing
#> # A tibble: 6 × 4
#>   variable prompt   label                                       base_ns
#>   <chr>    <chr>    <chr>                                         <int>
#> 1 var1     No label I like to talk about dogs                         6
#> 2 var1     No label Dogs are cool but cats are aight too              6
#> 3 var1     No label I prefer dogs over cats                           6
#> 4 var1     No label My dog's collars are always too tight             6
#> 5 var1     No label One last sentence about dogs                      6
#> 6 var1     No label Cats collars are typically cooler than dogs       6
```
