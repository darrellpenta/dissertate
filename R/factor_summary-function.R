#' Summary stats for factors
#'
#' @importFrom magrittr %>%
#' @inherit summary_stats
#' @param grp_factor a grouping factor
#' @param ... additional arguments
#' @return a data.frame of summary stats
#' @family factorial-design functions
#' @rdname factor_summary
#' @include summary_stats-function.R
#' @include factor_summary_helpers-function.R
#' @include sweet_dots-function.R
#' @include standard_error-function.R
#' @export
#'
factor_summary <- function(data, meas, cols, grp_factor,  ...) {
  assertthat::validate_that(
    is.data.frame(data) |
      tibble::is.tibble(data),
    assertthat::has_attr(data, "names")
  )
  assertthat::validate_that(!(missing(meas)))
  assertthat::validate_that(!(missing(grp_factor)),
                            is.character(grp_factor),
                            grp_factor %in% names(data))

  if (length(meas) == 1) {
    summary_out <-
      dplyr::bind_rows(
        dplyr::bind_cols(
          tibble::tibble("data" = grp_factor),
          summary_stats(data = dplyr::ungroup(data),
                        meas = meas)
        ),
        summary_subset(
          data = data,
          meas = meas,
          cols = cols
        ) %>%
          summary_combine()
      )


    summary_out <-
      dplyr::arrange_(summary_out,
                      .dots =
                        sweet_dots(paste0("dplyr::desc(",names(summary_out)[2],")")))

    summary_out
  } else {
    summary_list_out <-
      plyr::ldply(meas, function(meas_x,
                                 data_x = data,
                                 cols_x = cols,
                                 grp_factor_x = grp_factor) {
        summary_out <-
          dplyr::bind_rows(
            dplyr::bind_cols(
              tibble::tibble("data" = grp_factor_x),
              summary_stats(data = dplyr::ungroup(data_x),
                            meas = meas_x)
            ),
            summary_subset(
              data = data_x,
              meas = meas_x,
              cols = cols_x
            ) %>%
              summary_combine()
          )  %>%
          dplyr::mutate(Measure = paste0(meas_x))

        summary_out <-
          dplyr::arrange_(summary_out,
                          .dots =
                            sweet_dots(paste0("dplyr::desc(",names(summary_out)[2],")")))

        summary_out
      })
    summary_list_out
  }

}
