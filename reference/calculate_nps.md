# Quickly extract the Net Promoter Score (NPS) from a frequencies object of formatted NPS survey questions

Quickly extract the Net Promoter Score (NPS) from a frequencies object
of formatted NPS survey questions

## Usage

``` r
calculate_nps(
  frequencies,
  result = result,
  label = label,
  value = value,
  input_type = c("grouped", "numeric"),
  by_variable = TRUE,
  variable = variable,
  add_group = TRUE,
  get_brand = TRUE,
  prompt = prompt,
  brand_var_name = "brand",
  prompt_rm_pre = ".+\\,.+recommend ",
  prompt_rm_post = " to a .+\\? \\-.+",
  arrange_nps = TRUE,
  append_nps_to_brand = FALSE,
  brand_factor = TRUE
)
```

## Arguments

- frequencies:

  A frequencies table as produced by y2clerk::freqs()

- result:

  DEFAULT = result; The column of NPS question results (proportions)
  used to calculate the final NPS

- label:

  DEFAULT = label; The column of NPS question labels (responses) used to
  calculate the final NPS (if \`input_type\` is set to "grouped", this
  column MUST include the values "Detractor", "Passive", and "Promoter"
  and no other values)

- value:

  DEFAULT = value; The column of NPS question values (levels) used to
  order the final output

- input_type:

  DEFAULT = "grouped"; The input type of the NPS questions. Must be one
  of either "grouped" or "numeric" corresponding to either pre-formatted
  (3 groups of "Detractor", "Passive", and "Promoter") or unformatted
  (scale of 0-10) Qualtrics NPS questions, respectively

- by_variable:

  DEFAULT = TRUE; Boolean, controls whether the function calculates the
  NPS for each unique specified \`variable\` (question columns) or for
  the frequencies as a whole

- variable:

  DEFAULT = variable; The column of unique NPS questions and question
  names used to calculate the NPS for each (if \`by_variable\` is set to
  TRUE)

- add_group:

  DEFAULT = TRUE; Boolean, controls whether the function adds the
  specified \`variable\` as a grouping variable to the frequencies
  object (if it is already grouped) or if the specified \`variable\`
  will overwrite any and all previously applied grouping variables

- get_brand:

  DEFAULT = TRUE; Boolean, controls whether the function extracts the
  "brand" (NPS question subject) from the specified \`prompt\` column or
  does not

- prompt:

  DEFAULT = prompt; The column of NPS question prompts (question texts)
  from which to extract the "brand" (if \`get_brand\` is set to TRUE)

- brand_var_name:

  DEFAULT = "brand"; The name assigned to the output "brand" column (if
  \`get_brand\` is set to TRUE)

- prompt_rm_pre:

  DEFAULT = ".+\\.+recommend "; String pattern in the specified
  \`prompt\` column before which everything in the column is scrubbed to
  obtain the "brand"

- prompt_rm_post:

  DEFAULT = " to a .+\\ \\.+"; String pattern in the specified
  \`prompt\` column after which everything in the column is scrubbed to
  obtain the "brand"

- arrange_nps:

  DEFAULT = TRUE; Boolean, whether to arrange the final output by the
  NPS results, with previous frequencies arrangements/orderings still
  intact

- append_nps_to_brand:

  DEFAULT = FALSE; Boolean, whether to append the NPS values to the
  "brand" column, having the format "\["brand"\] (NPS = \[NPS\])"

- brand_factor:

  DEFAULT = TRUE; Boolean, whether to convert the "brand" variable (with
  appended NPS values) to a factor for ease of data visualization
  (argument specification only applied if \`append_nps_to_brand\` is set
  to TRUE)

## Value

An updated frequencies object with the new NPS column (and other
specified columns) attached, formatted as specified

## Examples

``` r
set.seed(1)

df <- data.frame(
  brand1_NPS_GROUP = sample(
    c(1:3, NA),
    size = 200,
    replace = TRUE
  ),
  brand2_NPS_GROUP = sample(
    c(1:3, NA),
    size = 200,
    replace = TRUE
  ),
  brand3_NPS_GROUP = sample(
    c(1:3, NA),
    size = 200,
    replace = TRUE
  )
) %>%
  labelled::set_value_labels(
    brand1_NPS_GROUP = c(
      'Promoter' = 3,
      'Passive' = 2,
      'Detractor' = 1
    ),
    brand2_NPS_GROUP = c(
      'Promoter' = 3,
      'Passive' = 2,
      'Detractor' = 1
    ),
    brand3_NPS_GROUP = c(
      'Promoter' = 3,
      'Passive' = 2,
      'Detractor' = 1
    )
  ) %>%
  labelled::set_variable_labels(
    brand1_NPS_GROUP = "How likely are you to recommend Brand1 to a friend or colleague? - Group",
    brand2_NPS_GROUP = "How likely are you to recommend Brand2 to a friend or colleague? - Group",
    brand3_NPS_GROUP = "How likely are you to recommend Brand3 to a friend or colleague? - Group"
  ) %>%
  tidyr::as_tibble()

# Frequencies
frequencies <- df %>%
  freqs(
    brand1_NPS_GROUP,
    brand2_NPS_GROUP,
    brand3_NPS_GROUP,
    nas = FALSE,
    prompt = TRUE
  )

# Calculate NPS
calculate_nps(frequencies)
#> # A tibble: 9 × 9
#>   variable         prompt             value label     n stat  result   nps brand
#>   <chr>            <chr>              <chr> <chr> <int> <chr>  <dbl> <dbl> <chr>
#> 1 brand1_NPS_GROUP How likely are yo… 1     Detr…    51 perc…   0.33     0 How …
#> 2 brand1_NPS_GROUP How likely are yo… 2     Pass…    54 perc…   0.35     0 How …
#> 3 brand1_NPS_GROUP How likely are yo… 3     Prom…    51 perc…   0.33     0 How …
#> 4 brand3_NPS_GROUP How likely are yo… 1     Detr…    61 perc…   0.41   -10 How …
#> 5 brand3_NPS_GROUP How likely are yo… 2     Pass…    41 perc…   0.28   -10 How …
#> 6 brand3_NPS_GROUP How likely are yo… 3     Prom…    46 perc…   0.31   -10 How …
#> 7 brand2_NPS_GROUP How likely are yo… 1     Detr…    58 perc…   0.39   -11 How …
#> 8 brand2_NPS_GROUP How likely are yo… 2     Pass…    48 perc…   0.33   -11 How …
#> 9 brand2_NPS_GROUP How likely are yo… 3     Prom…    41 perc…   0.28   -11 How …
```
