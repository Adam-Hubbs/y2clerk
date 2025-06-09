#' Title
#'
#' @param df A Tibble or object that can be coerced to a tibble
#' @param p A length 1+ vector of named prompts
#'
#' @returns A freq_y2 class (Subclass of a tibble)
#' @export
as_freq_y2 <- function(df, p = NULL) {
  if ('freq_y2' %in% class(df)) {
    return(df)
  }

  if (!('tbl_df' %in% class(df))) {
    df <- tibble::as_tibble(df)
  }


  if (!is.null(p)) {
    #that p is a named character vector
    if (!is.character(p)) {
      stop("p must be a character vector")
    }

    #Make sure that p is named for every element of p
    if (any(is.null(names(p))) | any(names(p) == "")) {
      stop("Every element of p must be named")
    }

    attr(df, "prompts") <- p
  }

  # IF the data frame is grouped, insert the freq_y2 class AFTER the grouped_df class.
  # This is necessary for proper dispatch.
  # We don't have to worry about grouped_freq_y2 already existing here becasue if 'freq_y2' was found earlier, it just returns it.
  if ('grouped_df' %in% class(df)) {
    class(df) <- c(
      'grouped_freq_y2',
      'grouped_df',
      'freq_y2',
      class(df)[class(df) != 'grouped_df']
    )
  } else {
    class(df) <- c("freq_y2", class(df))
  }

  return(df)
}


#' @exportS3Method dplyr::as_tibble
as_tibble.freq_y2 <- function(
  x,
  ...,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  attr(x, 'prompts') <- NULL
  NextMethod()
}

#' @exportS3Method
as.data.frame.freq_y2 <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  attr(x, 'prompts') <- NULL
  NextMethod()
}


#' @exportS3Method dplyr::group_by
group_by.freq_y2 <- function(.data, ..., add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  prompts <- attr(.data, "prompts")
  class(.data) <- setdiff(class(.data), "freq_y2")
  as_freq_y2(NextMethod(), p = prompts)
}

#' @exportS3Method dplyr::ungroup
ungroup.grouped_freq_y2 <- function(.data, ..., add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  prompts <- attr(.data, "prompts")
  class(.data) <- setdiff(class(.data), c("grouped_freq_y2", "freq_y2"))
  as_freq_y2(NextMethod(), p = prompts)
}


#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.grouped_freq_y2 <- function(data, cols) {
  prompts <- attr(.data, "prompts")

  out <- dplyr_col_modify(as_tibble(data), cols)
  if (any(names(cols) %in% group_vars(data))) {
    temp <- grouped_df(out, group_vars(data), drop = group_by_drop_default(data))
  } else {
    temp <- new_grouped_df(out, group_data(data))
  }
 as_freq_y2(temp, prompts)
}

dplyr_col_select <- function (.data, loc, error_call = caller_env())
{
  loc <- vec_as_location(loc, n = ncol(.data), names = names(.data))
  out <- .data[loc]

  print("Attributes after selection -------")
  print(attributes(out))

  if (!inherits(out, "data.frame")) {
    classes_data <- glue_collapse(class(.data), sep = "/")
    classes_out <- glue_collapse(class(out), sep = "/")
    bullets <- c("Can't reconstruct data frame.", x = glue("The `[` method for class <{classes_data}> must return a data frame."),
                 i = glue("It returned a <{classes_out}>."))
    abort(bullets, call = error_call)
  }
  if (length(out) != length(loc)) {
    classes_data <- glue_collapse(class(.data), sep = "/")
    classes_out <- glue_collapse(class(out), sep = "/")
    s <- function(x) if (length(x) == 1)
      ""
    else "s"
    bullets <- c("Can't reconstruct data frame.", x = glue("The `[` method for class <{classes_data}> must return a data frame with {length(loc)} column{s(loc)}."),
                 i = glue("It returned a <{classes_out}> of {length(out)} column{s(out)}."))
    abort(bullets, call = error_call)
  }
  if (identical(class(.data), "data.frame") || identical(class(.data),
                                                         c("data.table", "data.frame"))) {
    out <- dplyr_reconstruct(out, .data)
  }
  out
}

# dplyr_col_modify for Data.frame
# does a new_data_frame (vctrs) then a dplyr_reconstruct


# For grouped it does a col_modify (as_tibble(data)) then either does a grouped_df() or a new_grouped_df() [Both of which eventually end up as new_grouped_df]






