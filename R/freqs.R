##### Public functions #####

#' Run frequencies for multiple variables
#'
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataset. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param stat Character, stat to run. Currently accepts 'percent,' 'mean,' 'median,' 'min,' 'max,' 'quantile,' and 'summary' (default: 'percent').
#' @param percentile Double, for use when stat = 'quantile.' Input should be a real number x such that 0 <= x <= 100. Stands for percentile rank, which is a quantile relative to a 100-point scale. (default:NULL)
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @param unweighted_ns Boolean, whether the 'n' column in the freqs table should be UNweighted while results ARE weighted. This argument can only be used if a wt variable is used. If no weight variable is used, the 'n' column will always be unweighted (default: FALSE).
#' @param show_missing_levels Boolean, whether to keep response levels with no data (default: TRUE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations.
#' @importFrom rlang .data
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 2, 3, 4, 2, NA),
#'   b = c(1, 2, 2, 3, 4, 1, NA),
#'   weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#' freqs(df, a, b)
#' freqs(df, a, b, wt = weights)
#' freq(df, stat = 'mean', nas = FALSE)
#' freq(df, stat = 'mean', nas = FALSE, wt = weights)
#' df %>%
#'   dplyr::group_by(a) %>%
#'   freqs(b, nas = FALSE, wt = weights)
#'
#' # Note that percentile = 60 will return an estimate
#' # of the real number such that 60% of values
#' # are lower than that number
#'
#' # * note also that minimums and maximums are
#' # unaffected by weighting
#' freqs(df, a, stat = 'min', nas = FALSE)
#' freqs(df, a, stat = 'median', nas = FALSE)
#' freqs(df, a, stat = 'quantile', percentile = 95, nas = FALSE)
#' freqs(df, a, stat = 'summary', nas = FALSE, wt = weights)
#' @export

freqs <- function(
    dataset,
    ...,
    stat = c("percent", "mean", "median", "min", "max", "quantile", "summary"),
    percentile = NULL,
    nas = TRUE,
    wt = NULL,
    prompt = FALSE,
    digits = 2,
    nas_group = TRUE,
    factor_group = FALSE,
    unweighted_ns = FALSE,
    show_missing_levels = TRUE
) {
  # options(warn = -1)
  stat <- rlang::arg_match(stat)

  # Create logical for if there are weights
  weight_null <- dplyr::enquo(wt)
  weight_exists <- !rlang::quo_is_null(weight_null)

  if (unweighted_ns == TRUE & weight_exists == FALSE) {
    stop("If you use unweighted_ns = TRUE, you must specify a wt variable")
  } else if (unweighted_ns == TRUE & weight_exists == TRUE) {
    frequencies  <- freqs_wuw(
      dataset,
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = {{ wt }},
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    )
  } else {
    frequencies <- freqs_original(
      dataset,
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = {{ wt }},
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    )
  }

  #Attach question wordings
  if('variable' %in% names(frequencies)) {

    vars <- unique(frequencies$variable)
    p <- character(length(vars))
    names(p) <- vars

    for(var in vars) {
      p[var] <- attr(dataset[[var]], "label", exact = TRUE) %||% ""
    }

    p <- p[p != ""]
  }
  return(as_freq_y2(frequencies, p))
}

#' @rdname freqs
#' @export
freq <- freqs

# Private functions -------------------------------------------------------
# Freqs weighted results unweighted ns function
freqs_wuw  <- function(
    dataset,
    ...,
    stat,
    percentile,
    nas,
    wt,
    prompt,
    digits,
    nas_group,
    factor_group,
    show_missing_levels
) {

  # run weighted freqs
  freqs_weighted <-
    dataset %>%
    freqs_original(
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = {{ wt }},
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    ) %>%
    dplyr::select(-'n')

  # run unweighted freqs, but only keep n
  freqs_unweighted <-
    dataset %>%
    dplyr::select(-{{ wt }}) %>%
    freqs_original(
      ...,
      stat = stat,
      percentile = percentile,
      nas = nas,
      wt = NULL,
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group,
      show_missing_levels = show_missing_levels
    ) %>%
    dplyr::select('n')

  # Ungroup if freqs are grouped
  if (dplyr::is.grouped_df(freqs_unweighted)) {
    freqs_unweighted <- freqs_unweighted %>%
      dplyr::ungroup() %>%
      dplyr::select('n')
  }

  # bind freqs together
  frequencies <- dplyr::bind_cols(
    freqs_weighted,
    freqs_unweighted
  ) %>%
    dplyr::relocate(
      'n',
      .after = "label"
    )
  return(frequencies)
}



# Try including original freqs function as sub function
freqs_original <- function(
    dataset,
    ...,
    stat = stat,
    percentile = percentile,
    nas = nas,
    wt = wt,
    prompt = prompt,
    digits = digits,
    nas_group = nas_group,
    factor_group = factor_group,
    show_missing_levels = show_missing_levels
) {

  if (factor_group == TRUE){dataset <- group_factor(dataset)}
  if (nas_group == FALSE){dataset <- remove_group_nas(dataset)}
  weight <- dplyr::enquo(wt)
  variables <- dplyr::quos(...)

  # If no variables are specified in the function call,
  # assume the user wants to run a frequency on all columns.
  if (!length(variables)) {
    variables <- column_quos(dataset, wt = !!weight)
  }

  frequencies <- purrr::map_dfr(
    .x = variables,
    .f = function(variable) {
      freq_var(
        dataset,
        !!variable,
        stat,
        percentile,
        nas,
        !!weight,
        prompt,
        digits,
        show_missing_levels,
        nas_group
      )
    }
  )
  frequencies <- group_rename(frequencies)

  return(frequencies)
}



calculate_result_for_cont_var <- function(dataset, variable, stat, percentile, wt) {

  # first: (if wt = NULL) change class so logical test can be performed in all cases:
  if (base::is.null(wt)) {
    wt <- dplyr::enquo(wt)
  }

  # next: separate, verbose specifications for mean and quantile when weight is provided/not provided
  # (these if-else structures are inefficient but I wanted to be really clear about what we want)

  if (stat == 'mean') {
    # 1) wt = NULL
    if (rlang::quo_is_null(wt)) {
      out_df <- dataset %>%
        # always filter nas because the function previously checked
        # to ensure nas = FALSE is set if necessary
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = base::mean(!!variable)
        )
    }
    # 2) wt exists in dataset
    else {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::sum(!!wt),
                         result = stats::weighted.mean(!!variable,
                                                       !!wt)
        )
    }
  }

  if (stat %in% c('quantile', 'median', 'min', 'max')) {

    if (stat == 'median'){
      percentile <- 50
    } else if (stat == 'min') {
      percentile <- 0
    } else if (stat == 'max') {
      percentile <- 100
    }

    if (stat %in% c('min', 'max')) {
      # mins and maxes are never weighted, per our decision
      wt <- dplyr::quo(NULL)
    }

    # 1) wt = NULL
    if (rlang::quo_is_null(wt)) {
      out_df <- dataset %>%
        # always filter nas because the function previously checked
        # to ensure nas = FALSE is set if necessary
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = stats::quantile(x = !!variable,
                                                  probs = percentile / 100)
        )
    }
    # 2) wt exists in dataset
    else {
      out_df <- dataset %>%
        dplyr::filter(!is.na(!!variable)) %>%
        dplyr::summarise(n = base::length(!!variable),
                         result = reldist::wtd.quantile(!!variable,
                                                        q = percentile / 100,
                                                        weight = !!wt)
        )
    }
  }
  return(out_df)
}

validate_inputs <- function(dataset, variable, stat, percentile, nas, wt, prompt, digits) {

  # "failing fast"

  # 0) validate percentile rank
  if (stat == 'quantile' & is.null(percentile)) stop("No input given for percentile (percentile rank)")


  if (stat == 'quantile' & !is.null(percentile)) {
    if (percentile < 0 | percentile > 100) stop('Percentile rank should be between 0 and 100, inclusive')
  }

  if (stat == 'quantile' & !is.null(percentile)) {
    if (percentile < 1) rlang::inform('Remember that percentile ranges between 0 and 100. percentile = 0.5 returns the bottom half percentile, whereas percentile = 50 returns the median.')
  }

  # 1) if there are NAs in the data, you should use nas = FALSE
  if (nas) {
    count_nas <- dataset %>%
      dplyr::filter(is.na(!!variable)) %>%
      base::nrow()
    if (count_nas != 0) stop('NAs present in variable(s); to proceed, set nas = F')
  }

  # 2) can't take mean of categorical variable
  check_class <- dataset %>%
    dplyr::select(!!variable) %>%
    labelled::remove_labels() %>%
    dplyr::pull() %>%
    base::class()

  # make length = 1: collapse c("ordered", "factor") ==> c("ordered factor") as necessary
  check_class <- stringr::str_c(check_class, collapse = " ")

  # if not one of these types, stop
  if (! (check_class %in% c("numeric", "integer")) ) stop("Can't take mean of non-numeric variable")


  # 3) stop if value labels exist
  check_labels <- dataset %>%
    dplyr::ungroup() %>%
    dplyr::select(!!variable) %>%
    labelled::val_labels() %>%
    tibble::deframe() %>%
    base::is.null()
  if (! check_labels) stop("Value labels exist; consider converting values to labels or using stat = 'percent'")

  # 4) give reminder if percentile input given when stat is not set to 'quantile'
  if (!(stat %in% c('quantile', 'summary'))) {
    if (!is.null(percentile)) rlang::inform("Remember that the percentile rank argument impacts output only when stat = 'quantile'")
  }
}

get_output_for_cont_var <- function(dataset, variable, stat, percentile, nas, wt, prompt, digits) {

  # validation & checks
  validate_inputs(dataset,
                  variable,
                  stat,
                  percentile,
                  nas,
                  wt,
                  prompt,
                  digits)

  # get mean or quantile
  out_df <- calculate_result_for_cont_var(dataset,
                                          variable,
                                          stat,
                                          percentile,
                                          wt)

  # get group column names to add later (if they exist/as necessary)
  grouping_vars <- c(NULL)
  if (dplyr::is.grouped_df(dataset)) {
    grouping_vars <- dplyr::group_vars(dataset)
  }

  # produce dataframe to output

  # make copy of "stat". the stat variable in the output data frame and the
  # stat function argument don't play well together here.
  statistic <- stat
  rm(stat)
  # this is not a great fix imo but it's been a pretty resilient problem.
  # if possible, i would rename either the column or the argument, but
  # on the other hand, either of those would presumably be breaking changes

  # for convenience:
  if (is.null(percentile)) {
    percentile <- -99
  }

  out_df <- out_df %>%
    dplyr::mutate(variable = dplyr::quo_name(variable),
                  value = '',
                  label = '',
                  # different labels depending on input
                  stat = dplyr::case_when(
                    statistic == 'mean' ~ 'mean',
                    statistic == 'min' ~ 'min',
                    statistic == 'median' ~ 'median',
                    statistic == 'max' ~ 'max',
                    statistic == 'quantile' &
                      !(percentile %in% c(0,50,100)) ~ stringr::str_c('q', percentile),
                    statistic == 'quantile' & percentile == 0 ~ 'min',
                    statistic == 'quantile' & percentile == 50 ~ 'median',
                    statistic == 'quantile' & percentile == 100 ~ 'max',
                    TRUE ~ 'error'
                  ),
                  n = base::round(.data$n,
                                  digits),
                  result = base::round(.data$result,
                                       digits)) %>%
    dplyr::select(
      tidyselect::all_of(
        c(
          grouping_vars,
          'variable',
          'value',
          'label',
          'n',
          'stat',
          'result'
        )
      )
    ) %>%
    tibble::as_tibble()

  # fill out prompt column if specified
  if (prompt) {

    prompt_text <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::select(!!variable) %>%
      labelled::var_label() %>%
      tibble::deframe()

    # when prompt = TRUE but there is no variable label, output ""
    if (is.null(prompt_text)) {
      prompt_text <- ""
    }

    # final column ordering
    out_df <- out_df %>%
      dplyr::mutate(
        prompt = prompt_text
      ) %>%
      dplyr::select(
        tidyselect::all_of(
          c(
            grouping_vars,
            'variable',
            'prompt',
            'value',
            'label',
            'n',
            'stat',
            'result'
          )
        )
      )
  }

  # if weights are used, remove weight column rows from output
  if (!rlang::quo_is_null(wt)) {
    out_df <- out_df %>%
      dplyr::filter(variable != rlang::quo_name(wt))
  }

  # for convenience:
  if (percentile == -99) {
    percentile <- NULL
  }

  return(out_df)
}

get_summary_output_for_cont_var <- function(dataset, variable, stat, percentile, nas, wt, prompt, digits) {

  # add redundant reminder because  subsequent code overrides user inputs for stat & percentile
  # [for other cases, this reminder is also present in validate_inputs()]
  if (!is.null(percentile)) rlang::inform("Remember that the percentile rank argument impacts output only when stat = 'quantile'")

  out <- dplyr::bind_rows(
    get_output_for_cont_var(dataset, variable, stat = 'min', percentile,            nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'quantile', percentile = 25,  nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'median', percentile,         nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'mean', percentile,           nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'quantile', percentile = 75,  nas, wt, prompt, digits),
    get_output_for_cont_var(dataset, variable, stat = 'max', percentile,            nas, wt, prompt, digits)
  ) %>%
    dplyr::mutate(stat = forcats::fct_relevel(stat,
                                              c('min',
                                                'q25',
                                                'median',
                                                'mean',
                                                'q75',
                                                'max')
    )
    )

  return(out)
}

group_factor <- function(dataset){
  grouping_vars <- dplyr::group_vars(dataset)
  if (length(grouping_vars) > 0) { # 1 or more grouping vars
    group_flags <- list()
    for (grouping_var in grouping_vars) {
      group_flag <- grouping_var %>% as.symbol()
      group_flags <- c(group_flags, group_flag)
    }
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(grouping_vars),
          .fns = ~forcats::as_factor(.x)
        )
      )
    for (group_flag in group_flags) {
      dataset <- dataset %>%
        dplyr::group_by(
          !!group_flag,
          .add = TRUE
        )
    }
    return(dataset)
  } else { # Not grouped
    return(dataset)
  }
}

remove_group_nas <- function(dataset){
  grouping_vars <- dplyr::group_vars(dataset)
  if (length(grouping_vars) > 0){ # 1 or more grouping vars
    group_flags <- list()
    for (grouping_var in grouping_vars) {
      group_flag <- grouping_var %>% as.symbol()
      group_flags <- c(group_flags, group_flag)
    }
    for (group_flag in group_flags) {
      dataset <- dataset %>%
        dplyr::filter(
          !is.na(!!group_flag)
        )
    }
    return(dataset)
  } else { # Not grouped
    return(dataset)
  }
}

group_rename <- function(dataset){
  # Assumed, since non-percent calculations aren't grouped dataframes
  grouping_vars <- dataset %>%
    dplyr::select(
      -(tidyselect::all_of('variable'):dplyr::last_col())
    ) %>%
    names()

  if (length(grouping_vars) > 0){ # 1 or more grouping vars
    for (i in 1:length(grouping_vars)) {
      if (i == 1) {
        dataset <- dataset %>%
          dplyr::rename(group_var = grouping_vars[i])
      } else {
        dataset <- dataset %>%
          dplyr::rename(!!dplyr::sym(stringr::str_c('group_var', i)) := grouping_vars[i])
      }
    }
    return(dataset)
  } else { # Not grouped
    return(dataset)
  }
}

freq_var <- function(
    dataset,
    variable,
    stat = 'percent',
    percentile = 50,
    nas = TRUE,
    wt = NULL,
    prompt = FALSE,
    digits = 2,
    show_missing_levels = show_missing_levels,
    nas_group
) {
  variable <- dplyr::enquo(variable)
  wt <- dplyr::enquo(wt)

  # check stat argument input
  if (!(stat %in% c('percent','mean','quantile',
                    'summary','min','max','median'))) stop('"stat" argument must receive a value from c("percent", "mean", "quantile", "summary", "min", "median", "max")')

  if (stat == 'percent') {
    base <- ns(dataset, variable, wt, prompt, show_missing_levels, nas_group)
    freq_result <- base %>%
      percents(nas, digits = digits)
  }

  else if (stat %in% c('mean', 'quantile', 'min', 'median', 'max')) {
    freq_result <- get_output_for_cont_var(dataset, variable, stat, percentile, nas, wt, prompt, digits)
  }

  else if (stat == 'summary') {
    freq_result <- get_summary_output_for_cont_var(dataset, variable, stat, percentile, nas, wt, prompt, digits)
  }

  return(freq_result)
}

column_quos <- function(dataset, wt) {
  col_names <- dataset %>% colnames()
  if (dplyr::is.grouped_df(dataset)) {
    # Exclude grouping variables since they cannot be counted independent of groups.
    grouping_vars <- dplyr::group_vars(dataset)
    col_names <- setdiff(col_names, grouping_vars)
  }
  # Exclude weighting variable from freqs in select
  weight_name <- rlang::enquo(wt) %>% rlang::as_label()
  col_names <- setdiff(col_names, weight_name)

  col_syms <- col_names %>% dplyr::syms()
  col_quos <- purrr::map(col_syms, dplyr::quo)
  class(col_quos) <- append(class(col_quos),"quosures", after = 0)
  return(col_quos)
}

ns <- function(dataset, variable, weight, prompt, show_missing_levels, nas_group) {
  is_labelled <- sum(class(dataset %>% dplyr::pull(!!variable)) %in% c('labelled','haven_labelled','haven_labelled_spss'))
  counts <- if (is_labelled >= 1) {
    # Metadata is better if the given variable has labels
    labelled_ns(dataset, variable, weight, prompt, show_missing_levels, nas_group)
  } else {
    # Otherwise, use some sensible defaults
    unlabelled_ns(dataset, variable, weight, prompt)
  }
  # Reorder because Scotty is OCD
  if (prompt) {
    counts %>%
      dplyr::select(
        'variable',
        'prompt',
        'value',
        'label',
        'n'
      )
  } else {
    counts %>%
      dplyr::select(
        'variable',
        'value',
        'label',
        'n'
      )
  }
}

percents <- function(counts, include_nas, digits) {
  # Filter out NAs if requested
  if (! include_nas) {
    counts <- counts %>%
      dplyr::filter(
        !is.na(.data$value)
      )
  }
  # Calculate and round to integer percentages
  counts %>%
    dplyr::mutate(
      stat = 'percent',
      result = (.data$n / sum(.data$n)) %>% round(digits)
    )
}

labelled_ns <- function(dataset, variable, weight, prompt, show_missing_levels, nas_group) {
  # Extract the metadata from the labelled class
  counts <- base_ns(dataset, variable, weight)
  if (prompt) {
    prompt_text <- counts %>%
      dplyr::ungroup() %>%
      dplyr::select('value') %>%
      labelled::var_label() %>%
      as.character()
  }
  counts <- counts %>%
    dplyr::mutate(
      label = labelled::to_factor(.data$value) %>% as.character(),
      value = .data$value %>% as.character()
    )

  if (show_missing_levels == TRUE) {
    all_levels <- dataset %>%
      dplyr::pull(!!variable) %>%
      attributes() %>%
      purrr::pluck('labels')
    all_levels_tibble <- tibble::tibble(
      value = as.numeric(all_levels) %>% as.character(),
      label = names(all_levels) %>% as.character(),
      variable = rlang::quo_name(variable)
    )

    if (dplyr::is.grouped_df(dataset)) {
      grouping_vars <- dplyr::group_vars(dataset)
      all_group_levels <- dataset %>%
        dplyr::select(tidyselect::all_of(grouping_vars)) %>%
        dplyr::distinct()
      all_levels_tibble <- dplyr::cross_join(all_group_levels, all_levels_tibble)
      counts <- counts %>%
        dplyr::full_join(
          all_levels_tibble,
          by = c(grouping_vars, 'label', 'value', 'variable')
        ) %>%
        dplyr::mutate(
          n = ifelse(is.na(.data$n), 0, .data$n)
        )
      if (nas_group == FALSE) {
        counts <- counts %>%
          dplyr::filter_at(
            .vars = 1,
            ~!is.na(.)
          )
      }
    } else { # If not grouped
      counts <- counts %>%
        dplyr::full_join(
          all_levels_tibble,
          by = c('label', 'value', 'variable')
        ) %>%
        dplyr::mutate(
          n = ifelse(is.na(.data$n), 0, .data$n)
        )
    }
    counts <- counts %>% dplyr::arrange(.data$value)
  }

  if(prompt == TRUE) {
    counts$prompt <- prompt_text
  }

  return(counts)
}

unlabelled_ns <- function(dataset, variable, weight, prompt) {
  if (class(dataset %>% dplyr::pull(!!variable))[1] == 'factor') {
    counts <- base_ns(dataset, variable, weight) %>%
      dplyr::mutate(
        label = forcats::as_factor(.data$value) %>% as.character(),
        value = forcats::as_factor(.data$value) %>% as.numeric() %>% as.character()
      )
  } else {
    counts <- base_ns(dataset, variable, weight) %>%
      dplyr::mutate(
        label = .data$value %>% as.character(),
        value = .data$value %>% as.character()
      )
  }
  if (prompt) {
    counts <- counts %>%
      dplyr::mutate(
        prompt = ''
      )
  }
  return(counts)
}

base_ns <- function(dataset, variable, weight) {
  variable_label <- rlang::as_label(variable)
  dataset %>%
    # When wt is NULL, it runs unweighted counts
    dplyr::count(!!variable, wt = !!weight, .drop = TRUE) %>%
    dplyr::rename(value = !!variable) %>%
    dplyr::mutate(
      variable = variable_label
    )
}

