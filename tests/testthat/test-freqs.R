# test data ---------------------------------------------------------------

set.seed(100)
responses <- {
  data.frame(
    # continuous numeric, no variable label, no NA
    q0 = sample(
      x = datasets::swiss$Agriculture,
      size = 25,
      replace = TRUE
    ),
    # continuous numeric, variable label, incl. NA
    q1 = sample(
      x = c(datasets::swiss$Agriculture, NA),
      size = 25,
      prob = c(rep(.8 / 47, 47), 0.2),
      replace = TRUE
    ),
    # factor (numbers), no value labels
    q2 = sample(
      x = datasets::Orange$Tree,
      size = 25,
      replace = TRUE
    ),
    # character, no value labels
    q3 = sample(
      stringr::fruit,
      25,
      prob = 1 / (1:80 * sum(1 / (1:80))),
      replace = TRUE
    ),
    # numeric values, discrete numeric value labels
    q4 = sample(
      1:8,
      25,
      replace = TRUE
    ),
    # character values, discrete character value labels
    q5 = sample(
      letters[1:4],
      25,
      prob = c(0.4, 0.3, 0.2, 0.1),
      replace = TRUE
    ),
    # character, no value labels
    gender_labelled = c(
      rep(1, 12),
      rep(2, 12),
      rep(3, 0),
      rep(NA_real_, 1)
    ),
    # groups
    group_var1 = sample(
      c('group 1', 'group 2', NA_character_),
      25,
      prob = c(.8, .15, .05),
      replace = TRUE
    ),
    # numeric weights
    w = rnorm(25, mean = 1, sd = 0.1)
  ) %>%
    labelled::set_value_labels(
      q4 = c(
        `Less than a year` = 1,
        `1-2 years` = 2,
        `3-4 years` = 3,
        `5-10 years` = 4,
        `10-20 years` = 5,
        `20-50 years` = 6,
        `50-100 years` = 7,
        `More than 100 years` = 8
      ),
      q5 = c(
        `Very happy` = "a",
        `Somewhat happy` = "b",
        `Somewhat unhappy` = "c",
        `Very unhappy` = "d"
      ),
      gender_labelled = c(
        'male' = 1,
        'female' = 2,
        'other' = 3
      )
    ) %>%
    labelled::set_variable_labels(
      q1 = "% of males involved in agriculture",
      q2 = "Orange tree ID",
      q3 = "Preferred fruit",
      q4 = "Duration",
      q5 = "Satisfaction",
      w = "Weights",
      gender_labelled = 'gender'
    ) %>%
    dplyr::as_tibble()
}


# Basic tests -------------------------------------------------------------

### Incorrect parameter testing
#dataframes
test_that("Not a dataframe error - vectors", {
  df <- c('This', 'is', 'not', 'a', 'dataframe')
  a = c(1, 1, 2, 3, 1)
  expect_error(freqs(df, a))
})
test_that("Not a dataframe error - matrix", {
  column_a <- c(1, 1, 1, 1, 2, 2, 3)
  column_b <- c(0.5, 1.2, 0.8, 0.5, 0.2, 0.1, 1)
  table <- rbind(column_a, column_b)
  expect_error(freqs(table, column_a))
})
#variables
test_that("Runs on variables, not integers", {
  expect_error(freqs(mtcars, 10))
})
#nas
test_that("Incorrect nas argument", {
  expect_error(freqs(mtcars, cyl, nas = 'True'))
})
#weights
test_that("Incorrect wt argument", {
  expect_error(freqs(mtcars, cyl, wt = 'True'))
})

### frequency tibble class testing
#Returns class freq_y2
test_that("`freq()` returns a frequency tibble", {
  expect_s3_class(freqs(mtcars, cyl), 'freq_y2')
})

#Prints the question wordings
test_that("`freq()` prints question wordings", {
  test_freq1 <- responses |>
    dplyr::select(q1) |>
    freq()

  expect_output(print(test_freq1), 'q1: % of males involved in agriculture')
})

#Prints only three question wordings
test_that("`freq()` prints only three question wordings", {
  test_freq <- responses |>
    dplyr::select(q1, q2, q3, q4) |>
    freq()

  #Prints The question wordings for the first three
  expect_output(print(test_freq), 'q1: % of males involved in agriculture')
  expect_output(print(test_freq), 'q2: Orange tree ID')
  expect_output(print(test_freq), 'q3: Preferred fruit')

  # Does not print the question wording for the forth
  output <- capture.output(print(test_freq))
  expect_false(any(grepl("q4: Duration", output)))

  #Does print message saying how may questions have wordings that are not displayed
  expect_output(print(test_freq), '1 more questions with labels')
})

### weights
test_that("Weights", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )

  freqs_weighted <- freqs(df, a, wt = weights)
  expect_equal(freqs_weighted$n[1], .9)
})


### nas
#label
test_that("nas - label", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
  )

  yes_nas <- freqs(df, a)
  no_nas <- freqs(df, a, nas = FALSE)

  expect_equal(nrow(yes_nas), 5)
  expect_equal(nrow(no_nas), 4)
})
#group
test_that("nas - group", {
  df <- data.frame(
    a = c(1, 2, 2, 3, 4, 2, NA),
    a2 = c(1, 2, 2, 3, 4, 2, 5),
    g = c(1, 1, 2, 2, 3, NA, 2),
    g2 = c(1, 1, 2, 2, 3, 3, NA)
  ) %>%
    dplyr::group_by(g)

  yes_nas <- df %>%
    dplyr::group_by(g) %>%
    freqs(a)
  no_nas <- df %>%
    dplyr::group_by(g) %>%
    freqs(a, nas_group = FALSE)
  no_nas2 <- df %>%
    dplyr::group_by(g2) %>%
    freqs(a2, nas_group = FALSE)
  group_factors <- df %>%
    dplyr::group_by(g) %>%
    freqs(a, factor_group = TRUE)

  expect_equal(nrow(yes_nas), 7)
  expect_equal(nrow(no_nas), 6)
  expect_equal(is.factor(group_factors$group_var), TRUE)
  expect_equal(is.factor(no_nas$group_var), FALSE)
  expect_equal(names(yes_nas)[1], 'group_var')
})


###Digits
test_that("Digits", {
  df <- data.frame(
    a = c(.1, .2, .3)
  )

  dig1 <- freqs(df, a, digits = 1)
  dig2 <- freqs(df, a)
  dig3 <- freqs(df, a, digits = 3)

  expect_equal(dig1$result[1], .3)
  expect_equal(dig2$result[1], .33)
  expect_equal(dig3$result[1], .333)
})


###Differing classes of variables
#character column freq
test_that("character vars", {
  df <- data.frame(
    a = c('Character', '1', 'test')
  )
  frequencies <- freqs(df, a)
  expect_equal(is.data.frame(frequencies), TRUE)
})
#numeric column freq
test_that("numeric vars", {
  df <- data.frame(
    a = c(1, 2, 3)
  )
  frequencies <- freqs(df, a)
  expect_equal(is.data.frame(frequencies), TRUE)
})
#factored/labelled column freq
test_that("factor vars with missing values", {
  skip(message = 'Skipping for duckplyr test')

  df <- data.frame(
    a = c(1, 2, 3)
  )
  labelled::val_label(df$a, 1) <- 'Yes'
  labelled::val_label(df$a, 2) <- 'No'
  labelled::val_label(df$a, 3) <- 'Idk'
  labelled::val_label(df$a, 4) <- 'Do I show up?'
  df$a <- forcats::as_factor(df$a)

  frequencies <- freqs(df, a)
  expect_equal(nrow(frequencies), 4)
})

###Select function
#filter groups
test_that("filter out groups from vars", {
  df <- data.frame(
    a = c(1, 1, 3, 4, 5),
    b = c(1, 1, 1, 2, 2),
    c = c(2, 3, 4, 5, 6)
  )
  frequencies <- df %>%
    dplyr::select(a, b) %>%
    dplyr::group_by(b) %>%
    freqs()

  expect_equal(nrow(frequencies), 4)
})
#filter weights
test_that("filter out weights from vars", {
  df <- data.frame(
    a = c(1, 1, 3, 4, 5),
    b = c(1, 1, 1, 2, 2),
    c = c(2, 3, 4, 5, 6)
  )
  frequencies <- df %>%
    dplyr::select(a, b) %>%
    freqs(wt = b)

  expect_equal(nrow(frequencies), 4)
})

# More advanced tests -------------------------------------------------------------------

test_that("test data is correct", {
  expect_type(responses, "list")
  expect_equal(ncol(responses), 9)
  expect_equal(nrow(responses), 25)
})


test_that("NAs not present, nas = T: n & result are correct", {
  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "mean") %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    round(mean(responses$q0), 2)
  )

  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "mean", nas = TRUE) %>%
      dplyr::select(n) %>%
      dplyr::pull(),

    nrow(responses[!is.na(responses$q0), ])
  )
})

test_that("NAs not present, nas = F: n & result are correct", {
  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "mean", nas = FALSE) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    round(mean(responses$q0), 2)
  )
  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "mean", nas = FALSE) %>%
      dplyr::select(n) %>%
      dplyr::pull(),

    nrow(responses[!is.na(responses$q0), ])
  )
})

test_that("NAs present, nas = T: throws error", {
  expect_error(
    responses %>%
      dplyr::select(q1) %>%
      freqs(stat = "mean")
  )
})

test_that("NAs present, nas = F: n & result are correct", {
  expect_equal(
    responses %>%
      dplyr::select(q1) %>%
      freqs(stat = "mean", nas = FALSE) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    round(mean(responses$q1, na.rm = TRUE), 2)
  )
  expect_equivalent(
    responses %>%
      dplyr::select(q1) %>%
      freqs(stat = "mean", nas = FALSE) %>%
      dplyr::select(n) %>%
      dplyr::pull(),

    nrow(responses[!is.na(responses$q1), ])
  )
})


test_that("factor variable input: throws error", {
  expect_error(
    responses %>%
      select(q2) %>%
      freqs(stat = 'mean')
  )
})


test_that("character variable input: throws error", {
  expect_error(
    responses %>%
      dplyr::select(q3) %>%
      freqs(stat = 'mean')
  )
})


test_that("column with value labels input: throws error", {
  expect_error(
    responses %>%
      dplyr::select(q4) %>%
      freqs(stat = 'mean')
  )
})

test_that("column with value labels input: (potentially misleading) result is correct
          after labels are removed", {
  expect_equivalent(
    responses %>%
      dplyr::mutate(q4 = as.numeric(q4)) %>%
      dplyr::select(q4) %>%
      freqs(stat = 'mean') %>%
      dplyr::select(result) %>%
      dplyr::pull(),
    mean(responses$q4)
  )
})

test_that("column with value labels input: answer is correct after labels removed (even if potentially misleading)", {
  expect_equivalent(
    responses %>%
      dplyr::select(q4) %>%
      labelled::remove_labels() %>%
      freqs(stat = 'mean') %>%
      dplyr::select(result) %>%
      dplyr::pull(),
    mean(responses$q4)
  )
})


test_that("using weights: equivalent to weighted.mean() output", {
  expect_equal(
    responses %>%
      dplyr::select(q1, w) %>%
      freqs(stat = 'mean', nas = FALSE, wt = w) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    stats::weighted.mean(x = responses$q1, w = responses$w, na.rm = TRUE) %>%
      round(2)
  )
})


test_that("using prompt: variable label is correctly output", {
  expect_equal(
    responses %>%
      dplyr::select(q1) %>%
      freqs(stat = 'mean', nas = FALSE, prompt = TRUE) %>%
      dplyr::select(prompt) %>%
      dplyr::pull(),

    responses %>%
      dplyr::select(q1) %>%
      labelled::var_label() %>%
      tibble::deframe()
  )
})


test_that("using digits: output is precise to multiple decimal places", {
  expect_equal(
    responses %>%
      dplyr::select(w) %>%
      freqs(stat = 'mean', digits = 6, nas = FALSE) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    responses %>%
      dplyr::select(w) %>%
      dplyr::pull() %>%
      mean(na.rm = TRUE) %>%
      round(digits = 6)
  )
})


test_that("stat other than 'quantile' gives message when percentile value is provided", {
  expect_message(
    responses %>%
      freqs(q1, percentile = 75, stat = 'mean', nas = FALSE)
  )
})

test_that("stat argument only accepts percent, mean, quantile, or summary", {
  expect_error(
    responses %>%
      freqs(q1, stat = 'means', percentile = 75, nas = FALSE)
  )
})

test_that("function stops when value labels exist", {
  expect_error(
    responses %>%
      freqs(q4, stat = 'mean', nas = FALSE)
  )
})


test_that("unweighted_ns = TRUE, but no wt variable", {
  expect_error(
    responses %>%
      freqs(
        q4,
        unweighted_ns = TRUE
      ),
    "If you use unweighted_ns = TRUE, you must specify a wt variable"
  )
})

test_that("freqs_wuw, ns and results are equal", {
  freqs_normal <- mtcars %>% freqs(gear)
  freqs_normal_weighted <- mtcars %>% freqs(gear, wt = carb)
  freqs_wuw_table <- mtcars %>%
    y2clerk:::freqs_wuw(
      gear,
      wt = carb,
      # Defaults auto input by function
      stat = 'percent',
      percentile = NULL,
      nas = TRUE,
      prompt = FALSE,
      digits = 2,
      nas_group = TRUE,
      factor_group = FALSE,
      show_missing_levels = FALSE
    )
  freqs_wuw_infreqs <- mtcars %>%
    freqs(
      gear,
      wt = carb,
      unweighted_ns = TRUE,
      show_missing_levels = FALSE
    )

  expect_equal(freqs_normal_weighted$result, freqs_wuw_infreqs$result)
  expect_equal(freqs_normal$n, freqs_wuw_infreqs$n)
})


test_that("freqs_wuw, test on responses", {
  freqs_normal <- responses %>% freqs(q4)
  freqs_normal_weighted <- responses %>% freqs(q4, wt = w)
  freqs_wuw_table <- responses %>%
    y2clerk:::freqs_wuw(
      q4,
      wt = w,
      # Defaults auto input by function
      stat = 'percent',
      percentile = NULL,
      nas = TRUE,
      prompt = FALSE,
      digits = 2,
      nas_group = TRUE,
      factor_group = FALSE,
      show_missing_levels = FALSE
    )
  freqs_wuw_infreqs <- responses %>%
    freqs(
      q4,
      wt = w,
      unweighted_ns = TRUE,
      show_missing_levels = FALSE
    )

  expect_equal(freqs_normal_weighted$result, freqs_wuw_infreqs$result)
  expect_equal(freqs_normal$n, freqs_wuw_infreqs$n)
})


test_that("multiple group_vars", {
  frequencies <- responses %>%
    dplyr::group_by(
      group_var1,
      gender_labelled
    ) %>%
    freqs(
      q4,
      nas_group = FALSE
    )

  possible_combs <- responses %>%
    dplyr::select(
      group_var1,
      gender_labelled,
      q4
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(q4 = as.numeric(q4)) %>%
    dplyr::arrange(
      group_var1,
      gender_labelled,
      q4
    ) %>%
    tidyr::drop_na()

  calculated_combs <- frequencies %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 0) %>%
    dplyr::select(
      group_var1 = group_var,
      gender_labelled = group_var2,
      q4 = value
    ) %>%
    dplyr::mutate(q4 = as.numeric(q4)) %>%
    dplyr::arrange(
      group_var1,
      gender_labelled,
      q4
    ) %>%
    dplyr::as_tibble()

  expect_equal(possible_combs, calculated_combs)
})


# Test on show missing levels ---------------------------------------------
test_that("multi_freqs - show_missing_levels argument", {
  test_no_missing_levels <- responses %>%
    freqs(
      gender_labelled,
      show_missing_levels = FALSE
    )
  test_yes_missing_levels <- responses %>%
    freqs(
      gender_labelled,
      show_missing_levels = TRUE
    )
  test_yes_missing_levels_no_nas <- responses %>%
    freqs(
      gender_labelled,
      nas = FALSE,
      show_missing_levels = TRUE
    )
  sum_no_missing <-
    stringr::str_detect(test_no_missing_levels$label, 'other') %>%
    sum(na.rm = TRUE)
  sum_yes_missing <-
    stringr::str_detect(test_yes_missing_levels$label, 'other') %>%
    sum(na.rm = TRUE)
  sum_yes_missing_nas <-
    stringr::str_detect(test_yes_missing_levels_no_nas$label, 'other') %>%
    sum()

  expect_equal(sum_no_missing, 0)
  expect_equal(sum_yes_missing, 1)
  expect_equal(sum_yes_missing_nas, 1)
})


test_that("freqs - show_missing_levels argument", {
  # Missing level shows up in NA group, but not other groups
  no_missing <- responses %>%
    dplyr::group_by(group_var1) %>%
    freqs(
      gender_labelled,
      nas = FALSE,
      show_missing_levels = FALSE
    )
  yes_missing <- responses %>%
    dplyr::group_by(group_var1) %>%
    freqs(
      gender_labelled,
      nas = FALSE,
      show_missing_levels = TRUE
    )
  yes_missing_no_nas_group <- responses %>%
    dplyr::group_by(group_var1) %>%
    freqs(
      gender_labelled,
      nas = FALSE,
      show_missing_levels = TRUE,
      nas_group = FALSE
    )
  sum_no_missing <-
    stringr::str_detect(no_missing$label, 'other') %>%
    sum()
  sum_yes_missing <-
    stringr::str_detect(yes_missing$label, 'other') %>%
    sum()
  sum_yes_missing_no_nas_group <-
    stringr::str_detect(yes_missing_no_nas_group$label, 'other') %>%
    sum()

  expect_equal(sum_no_missing, 0)
  expect_equal(sum_yes_missing, 3)
  expect_equal(sum_yes_missing_no_nas_group, 2)
})

test_that("freqs - show_missing_levels ordered", {
  missing_tibble <- tibble::tibble(
    weekdays = c(
      rep(1, 10),
      rep(2, 0),
      rep(3, 10),
      rep(4, 0),
      rep(5, 10)
    ),
    pokemon = c(
      rep(1, 12),
      rep(2, 5),
      rep(3, 13)
    )
  ) %>%
    labelled::set_value_labels(
      weekdays = c(
        'Monday' = 1,
        'Tuesday' = 2,
        'Wednesday' = 3,
        'Thursday' = 4,
        'Friday' = 5
      ),
      pokemon = c(
        'Bulbasaur' = 1,
        'Charmander' = 2,
        'Squirtle' = 3
      )
    )

  missing_freqs <- missing_tibble %>% freqs(weekdays)
  missing_freqs_grouped <- missing_tibble %>%
    dplyr::group_by(pokemon) %>%
    freqs(weekdays, factor_group = TRUE)

  expect_equal(missing_freqs %>% dplyr::pull(n), c(10, 0, 10, 0, 10))
  expect_equal(
    missing_freqs %>% dplyr::pull(label),
    c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
  )
  expect_equal(
    missing_freqs_grouped %>% dplyr::pull(label) %>% unique(),
    c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
  )
})


# Percentile tests --------------------------------------------------------

test_that("bad input throws error", {
  expect_error(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "perc")
  )
})


test_that("NAs not present, nas = T: n & result are correct", {
  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "quantile", percentile = 95) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    round(quantile(responses$q0, 0.95), 2)
  )

  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "mean", nas = TRUE) %>%
      dplyr::select(n) %>%
      dplyr::pull(),

    nrow(responses[!is.na(responses$q0), ])
  )
})

test_that("NAs not present, nas = F: n & result are correct", {
  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "quantile", nas = FALSE, percentile = 50) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    round(median(responses$q0), 2)
  )
  expect_equivalent(
    responses %>%
      dplyr::select(q0) %>%
      freqs(stat = "quantile", nas = FALSE, percentile = 50) %>%
      dplyr::select(n) %>%
      dplyr::pull(),

    nrow(responses[!is.na(responses$q0), ])
  )
})

test_that("NAs present, nas = T: throws error", {
  expect_error(
    responses %>%
      dplyr::select(q1) %>%
      freqs(stat = "quantile", percentile = 95)
  )
})

test_that("NAs present, nas = F: n & result are correct", {
  expect_equal(
    responses %>%
      dplyr::select(q1) %>%
      freqs(stat = "quantile", nas = FALSE, percentile = 95) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    round(quantile(responses$q1, 0.95, na.rm = TRUE), 2)
  )
  expect_equivalent(
    responses %>%
      dplyr::select(q1) %>%
      freqs(stat = "quantile", nas = FALSE, percentile = 95) %>%
      dplyr::select(n) %>%
      dplyr::pull(),

    nrow(responses[!is.na(responses$q1), ])
  )
})


test_that("factor variable input: throws error", {
  expect_error(
    responses %>%
      dplyr::select(q2) %>%
      freqs(stat = 'quantile')
  )
})


test_that("character variable input: throws error", {
  expect_error(
    responses %>%
      dplyr::select(q3) %>%
      freqs(stat = 'quantile')
  )
})


test_that("column with value labels input: throws error", {
  expect_error(
    responses %>%
      dplyr::select(q4) %>%
      freqs(stat = 'quantile')
  )
})

test_that("column with value labels input: (potentially misleading) result is correct
          after labels are removed", {
  expect_equivalent(
    responses %>%
      dplyr::mutate(q4 = as.numeric(q4)) %>%
      dplyr::select(q4) %>%
      freqs(stat = 'quantile', percentile = 95) %>%
      dplyr::select(result) %>%
      dplyr::pull(),
    quantile(as.numeric(responses$q4), 0.95)
  )
})

test_that("column with value labels input: answer is correct after labels removed (even if potentially misleading)", {
  expect_equivalent(
    responses %>%
      dplyr::select(q4) %>%
      labelled::remove_labels() %>%
      freqs(stat = 'quantile', percentile = 95) %>%
      dplyr::select(result) %>%
      dplyr::pull(),
    quantile(as.numeric(responses$q4), 0.95)
  )
})


test_that("using weights: equivalent to wtd.quantile() output", {
  expect_equal(
    responses %>%
      dplyr::select(q1, w) %>%
      freqs(stat = 'quantile', nas = FALSE, wt = w, percentile = 95) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    reldist::wtd.quantile(
      x = responses$q1,
      q = 0.95,
      weight = responses$w,
      na.rm = TRUE
    ) %>%
      round(2)
  )
})


test_that("using prompt: variable label is correctly output", {
  expect_equal(
    responses %>%
      dplyr::select(q1) %>%
      freqs(
        stat = 'quantile',
        nas = FALSE,
        prompt = TRUE,
        percentile = 0.95
      ) %>%
      dplyr::select(prompt) %>%
      dplyr::pull(),

    responses %>%
      dplyr::select(q1) %>%
      labelled::var_label() %>%
      tibble::deframe()
  )
})


test_that("using digits: output is precise to multiple decimal places", {
  expect_equal(
    responses %>%
      dplyr::select(w) %>%
      freqs(stat = 'quantile', percentile = 95, digits = 6, nas = FALSE) %>%
      dplyr::select(result) %>%
      dplyr::pull(),

    responses %>%
      dplyr::select(w) %>%
      dplyr::pull() %>%
      quantile(0.95, na.rm = TRUE) %>%
      round(digits = 6)
  )
})


test_that("output from 'percentile = 0' is equivalent to base::min()", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'quantile', percentile = 0) %>%
      dplyr::select(result) %>%
      dplyr::pull() %>%
      as.numeric(),

    min(responses$q0)
  )
})


test_that("output from 'percentile = 100' is equivalent to base::max()", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'quantile', percentile = 100) %>%
      dplyr::select(result) %>%
      dplyr::pull() %>%
      as.numeric(),

    max(responses$q0)
  )
})


test_that("there are 6 lines of output (min, 1st quartile, median, mean, 3rd quartile, max)", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'summary') %>%
      nrow(),
    6
  )

  expect_equal(
    responses %>%
      freqs(q1, stat = 'summary', nas = FALSE) %>%
      nrow(),
    6
  )

  expect_equal(
    responses %>%
      freqs(q1, stat = 'summary', nas = FALSE, wt = w) %>%
      nrow(),
    6
  )
})

test_that("setting a percentile value when stat = 'summary' does not affect output", {
  expect_equal(
    responses %>%
      freqs(q0, stat = 'summary', percentile = 0),
    responses %>%
      freqs(q0, stat = 'summary')
  )
})

test_that("stat = 'summary' gives message when percentile value is provided", {
  expect_message(
    responses %>%
      dplyr::select(q0, q1, w) %>%
      freqs(percentile = 75, stat = 'summary', wt = w, nas = FALSE)
  )
})

test_that("stat = 'mean' works when GROUPED", {
  test <- responses %>%
    dplyr::group_by(q2) %>%
    freqs(q1, stat = "mean", nas = FALSE, wt = q0)

  expect_equal(length(test$group_var), 4)
})
