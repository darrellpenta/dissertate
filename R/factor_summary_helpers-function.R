#' Helper functions for creating factor_summary_data
#'
#' @inheritParams summary_stats
#' @param ... ignored arguments to other functions
#' @rdname factor_summary_helpers
#' @include combine_factor_levels-function.R
#' @include select_dots-function.R
#' @include standard_error-function.R

#' Summarize with subsets
#' @rdname factor_summary_helpers
#' @export

summary_subset <- function(data, cols, meas, ...) {
  sub_set_out <-
    combine_factor_levels(
      tibble::tibble("data" = cols),
      m = length(cols):1,
      simplify = FALSE,
      byrow = TRUE
    ) %>%
    unlist(recursive = FALSE,
           use.names = FALSE) %>%
    lapply(function(cols_,
                    dat = data,
                    meas_ = meas) {
      summary_stats(data = dplyr::ungroup(dat),
                    meas = meas_,
                    cols = cols_)

    })
  sub_set_out
}

#' Combine summaries
#' @rdname factor_summary_helpers
#' @export
#'

summary_combine <- function(data, ...) {
  combine_out <-
    plyr::ldply(data, function(x) {
      x_out_vector <-
        names(x)[grepl("[^N|M|SD|SE]", names(x), ignore.case = FALSE)]

      if (ncol(x) != 1) {
        x_out <-
          tidyr::unite_(
            x,
            col = "data",
            from = x_out_vector,
            sep = ":",
            remove = TRUE
          )
        x_out
      } else {
        x_out <-
          tidyr::unite_(
            x,
            col = "data",
            from = x_out_vector,
            sep = "",
            remove = TRUE
          )
        x_out
      }
      x_out

    })
  combine_out
}
