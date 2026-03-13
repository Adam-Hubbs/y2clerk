# Widen a frequencies table

Given a grouped frequencies table, pivot_freqs will create new columns
for each label level in the frequencies

## Usage

``` r
pivot_freqs(dataset, columns_var = label)
```

## Arguments

- dataset:

  A grouped frequencies table as produced by y2clerk::freqs()

- columns_var:

  DEFAULT = label; If label, the frequencies will be pivoted so a new
  column will be created for each unique level of label. Can also be set
  to group_var to pivot the other way and create new columns for each
  unique level of group_var

## Value

A wide tibble of frequencies with one row for each group (by default)

## Examples

``` r
  frequencies <- forcats::gss_cat %>%
    dplyr::group_by(year) %>%
      y2clerk::freqs(marital) %>%
      pivot_freqs()
#> Adding missing grouping variables: `year`
```
