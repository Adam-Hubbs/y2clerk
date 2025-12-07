
### Column names
test_that("Column names", {
  frequencies_base <- forcats::gss_cat %>%
    dplyr::group_by(year) %>%
    freqs(marital)
  frequencies_base

  frequencies_pivoted <- frequencies_base %>% pivot_freqs()
  frequencies_pivoted

  names_actual <- frequencies_pivoted %>% names()
  names_expected <- c(
    'group_var',
    'No answer',
    'Never married',
    'Separated',
    'Divorced',
    'Widowed',
    'Married'
  )
  expect_equal(names_actual, names_expected)
})


test_that("Column names with two group vars", {
  skip(message = 'Skipping for duckplyr test')
  
  frequencies_pivoted <- forcats::gss_cat %>%
    dplyr::group_by(year) %>%
    freqs(race) %>%
    pivot_freqs()

  frequencies_pivoted
  nrows <- nrow(frequencies_pivoted)
  names_cols <- names(frequencies_pivoted)

  names_actual <- frequencies_pivoted %>% names()
  names_expected <- c(
    'group_var',
    'Other',
    'Black',
    'White',
    'Not applicable'
  )
  expect_equal(names_actual, names_expected)
})

### Group_var levels
test_that("Each row is a group_var level", {
  frequencies_pivoted <- forcats::gss_cat %>%
    dplyr::group_by(year) %>%
    freqs(marital) %>%
    pivot_freqs()
  frequencies_pivoted
  nrows <- length(frequencies_pivoted$group_var)
  names_rows <- as.character(frequencies_pivoted$group_var)

  expect_equal(nrows, 8)
  expect_equal(names_rows, c('2000',
                             '2002',
                             '2004',
                             '2006',
                             '2008',
                             '2010',
                             '2012',
                             '2014'))
})


### columns_var - pivot other way
test_that("Pivot on group_var", {
  frequencies_pivoted <- forcats::gss_cat %>%
    dplyr::group_by(year) %>%
    freqs(marital) %>%
    pivot_freqs(group_var)
  frequencies_pivoted
  nrows <- nrow(frequencies_pivoted)
  names_cols <- names(frequencies_pivoted)

  expect_equal(nrows, 6)
  expect_equal(names_cols, c('label', '2000', '2002', '2004', '2006',
                             '2008', '2010', '2012', '2014'))
})


### Errors
test_that("pivot_freqs error testing - blank label column", {
  expect_error(
    frequencies <- forcats::gss_cat %>%
      freqs(age, stat = 'mean', nas = FALSE) %>%
      pivot_freqs(),
    'Your frequencies label column is blank. Please provide labels on which to pivot.',
    fixed = TRUE
  )
})

test_that("pivot_freqs error testing - missing group_var", {
  expect_error(
    frequencies <- forcats::gss_cat %>%
      freqs(marital) %>%
      pivot_freqs(),
    'Your frequencies does not contain a group_var. It must have a group_var to pivot correctly.',
    fixed = TRUE
  )
})

test_that("pivot_freqs error testing - missing label or result column", {
  expect_error(
    frequencies <- forcats::gss_cat %>%
      pivot_freqs(),
    'Input data must contain a "label" column and a "result" column. Ensure you are passing the output from a freqs() call.',
    fixed = TRUE
  )
})
