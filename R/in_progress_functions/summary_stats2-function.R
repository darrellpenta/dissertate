#' Standard summary stats
#'
#' This is the generic version of the \code{summary_stats}
#'
#' @importFrom magrittr %>%
#' @param .data for which a summary is to be computed
#' @param ... additional arguments
#' @param .dep_var dependent measure to compute summaries over
#' @param .ind_var FALSE (default) for collapsing over all variables, or a vector of variables to collapses over in stages
#' @return a data frame with columns for:  N = length, M = mean, SD = sd, SE = standard_error
#' @family aov functions
#' @include sweet_dots-function.R
#' @include standard_error-function.R
#' @rdname summary_stats2
#' @export
#'
summary_stats_ <-function(.data, ...){
  UseMethod("summary_stats_", .data)
}


#' Summary stats for data.frame class
#'
#' @rdname summary_stats2
#' @export
summary_stats_.data.frame <-
  function(.data, ...,.dep_var,.ind_var = FALSE) {
  assertthat::validate_that(
    is.data.frame(.data) |
      tibble::is.tibble(.data),
    assertthat::has_attr(.data, "names")
  )
  assertthat::validate_that(!(missing(.dep_var)))

if(is.character(.ind_var))
{ .data<-
  dplyr::group_by_(.data = .data,
                            .dots = sweet_dots(.ind_var))
.data} else {
  .data <-
    .data
}

  standard_error <- function(x) {
    x <-
      stats::sd(x) / sqrt(length(x))
  }
  sd <- stats::sd

  data_out <-
    dplyr::summarise_at(.data, c(paste0(.dep_var)), .funs  = dplyr::funs(
      N = length,
      M = mean,
      SD = sd,
      SE = standard_error
    ))

  data_out

  }


#' A vectorized version of summary_stats.data.frame
#' @rdname summary_stats2
#' @export
summary_stats_.default <-
  Vectorize(
    FUN = function(.data, .dep_var, .ind_var, ...)
      summary_stats_.data.frame(.data = .data,
                               ...,
                               .ind_var = .ind_var,
                               .dep_var = .dep_var),
    vectorize.args = c(".data", ".dep_var"),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

