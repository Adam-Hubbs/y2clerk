# Silence CRAN check over missing global binding (because we make it when the user prints a frequency tibble
if(getRversion() >= "2.15.1") utils::globalVariables(c(".print_buffer"))

#' @exportS3Method
print.freq_y2 <- function(x, n, ...) {
  create_env_in_global()
  NextMethod()
}


#' Print with n = Inf
#'
#' @description
#' This function prints the most recently displayed frequency tibble with print(n = Inf)
#'
#' @export
print_freq_inf <- function() {
  print(.print_buffer$last_freq, n = Inf)
}


### These functions all override a part of printing frequency tibbles.
### The tibble printing flow is extremely complicated and powerful.
### It allows overriding parts of the tibble printing while not providing an entirely custom solution.

#' @exportS3Method pillar::tbl_sum
tbl_sum.freq_y2 <- function(x, ...) {
  c(`A frequency tibble` = pillar::dim_desc(x))
}


#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.freq_y2 <- function(x, setup, ...) {
  footer <- pillar:::format_footer(x, setup)
  footer_comment <- pillar:::wrap_footer_bullet(footer, setup)
  footer_advice <- format_footer_advice.freq_y2(x, setup)
  footer_advice_comment <- pillar:::wrap_footer_bullet(
    footer_advice,
    setup,
    lines = 1,
    ellipsis = FALSE,
    bullet = cli::symbol$info
  )
  pillar::style_subtle(c(
    footer_comment,
    pillar:::format_comment(footer_advice_comment, width = setup$width)
  ))
}


#' @exportS3Method pillar::format_footer_advice
format_footer_advice.freq_y2 <- function(x, setup) {
  if (!isTRUE(pillar:::pillar_options$advice())) {
    return()
  }
  if (setup$extra_cols_total > length(setup$extra_cols)) {
    cols <- "`colnames()` to see all variable names"
  } else {
    cols <- NULL
  }
  if (is.na(setup$rows_missing) || setup$rows_missing > 0) {
    .print_buffer$last_freq <- x

    rows <- cli::cli_text(cli::col_silver(
      "# {cli::symbol$info} Use `{.run [print(n = ...)](y2clerk::print_freq_inf())}` to see more rows"
    ))
    ###cli_text ends this function
  } else {
    rows <- NULL
  }
  advice <- pillar:::enum_collapse(c(rows, cols))
  if (length(advice) == 0) {
    return()
  }
  paste0("Use ", paste(advice, collapse = " "))
}


#' @exportS3Method pillar::tbl_format_header
tbl_format_header.freq_y2 <- function(x, setup, ...) {
  named_header <- setup$tbl_sum
  focus <- attr(x, "pillar_focus")
  if (!is.null(focus)) {
    named_header <- c(
      named_header,
      `Focus columns` = pillar:::collapse(pillar:::tick_if_needed(focus))
    )
  }
  if (all(rlang::names2(named_header) == "")) {
    header <- named_header
  } else {
    header <- paste0(
      pillar::align(paste0(rlang::names2(named_header), ":"), space = "\U00A0"),
      " ",
      named_header
    )
  }

  qs <- get_question_wordings(x, setup)

  pillar::style_subtle(c(
    qs,
    pillar:::format_comment(header, width = setup$width)
  ))
}


get_question_wordings <- function(x, setup) {
  #Question Names
  p <- attr(x, "prompts")
  qs <- character()

  if (length(p) > 0) {
    max_iter <- min(3, length(p))

    for (i in seq_len(max_iter)) {
      qs[i] <- pillar::style_subtle(
        pillar:::format_comment(
          paste0(cli::style_underline(names(p)[i]), ": ", p[i], "\n", sep = ""),
          width = setup$width
        )
      )
    }
    questions_left <- length(p) - max_iter
    #display a message that says: X more questions with labels
    if (questions_left > 0) {
      qs[max_iter + 1] <- pillar::style_subtle(
        pillar:::format_comment(
          paste0(
            cli::symbol$info,
            " ",
            questions_left,
            " more questions with labels\n",
            sep = ""
          ),
          width = setup$width
        )
      )
      qs[max_iter + 2] <- pillar::style_subtle(
        pillar:::format_comment(
          "\n",
          width = setup$width
        )
      )
    } else {
      qs[max_iter + 1] <- pillar::style_subtle(
        pillar:::format_comment(
          "\n",
          width = setup$width
        )
      )
    }
  }
  return(qs)
}


#' Create print buffer environment if it does not exist
create_env_in_global <- function() {
  if (!exists(".print_buffer", envir = .GlobalEnv)) {
    eval(
      quote(.GlobalEnv$.print_buffer <- new.env(parent = emptyenv())),
      envir = .GlobalEnv
    )
  }
}
