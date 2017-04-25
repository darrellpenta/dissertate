#' Summary stats for factors
#'
#' @importFrom magrittr %>%
#' @inherit summary_stats
#' @param .grp_var a grouping factor
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
factor_summary <- function(.data, .dep_var, .ind_var, .grp_var,  ...) {
  assertthat::validate_that(
    is.data.frame(.data) |
      tibble::is.tibble(.data),
    assertthat::has_attr(.data, "names")
  )
  assertthat::validate_that(!(missing(.dep_var)))
  assertthat::validate_that(!(missing(.grp_var)),
                            is.character(.grp_var),
                            .grp_var %in% names(.data))

  if (length(.dep_var) == 1) {
    summary_out <-
      dplyr::bind_rows(
        dplyr::bind_cols(
          tibble::tibble("data" = .grp_var),
          summary_stats(.data = dplyr::ungroup(.data),
                        .dep_var = .dep_var)
        ),
        summary_subset(
          .data = .data,
          .dep_var = .dep_var,
          .ind_var = .ind_var
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
      plyr::ldply(.dep_var, function(dep_var_x,
                                 data_x = .data,
                                 cols_x = .ind_var,
                                 grp_var_x = .grp_var) {
        summary_out <-
          dplyr::bind_rows(
            dplyr::bind_cols(
              tibble::tibble("data" = grp_var_x),
              summary_stats(.data = dplyr::ungroup(data_x),
                            .dep_var = dep_var_x)
            ),
            summary_subset(
              .data = data_x,
              .dep_var = dep_var_x,
              .ind_var = cols_x
            ) %>%
              summary_combine()
          )  %>%
          dplyr::mutate(Measure = paste0(dep_var_x))

        summary_out <-
          dplyr::arrange_(summary_out,
                          .dots =
                            sweet_dots(paste0("dplyr::desc(",names(summary_out)[2],")")))

        summary_out
      })
    summary_list_out
  }

}
