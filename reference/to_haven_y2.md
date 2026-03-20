# Convert a variable to haven labelled

Convert a character or factor vector into a dbl labelled (haven
labelled) vector. Useful for when you need to either extract labels or
attach underlying numbers to each label

## Usage

``` r
to_haven_y2(variable)
```

## Arguments

- variable:

  The vector or variable within a data frame that you wish to convert to
  haven labelled

## Examples

``` r
test <- tibble::tibble(
  color_factor = c('Blue', 'Blue', 'Red', 'Yellow') |>
    forcats::as_factor()
)

test$color <- to_haven_y2(test$color_factor)
test <- test |>
  dplyr::mutate(
    color = to_haven_y2(color_factor)
  )
```
