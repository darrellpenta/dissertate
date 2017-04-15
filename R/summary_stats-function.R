#' Standard summary stats
#'
#' This is the generic version of the \code{summary_stats}
#'
#' @importFrom magrittr %>%
#' @param data for which a summary is to be computed
#' @param ... additional arguments
#' @return a data frame with columns for:  N = length, M = mean, SD = sd, SE = standard_error
#' @family aov functions
#' @include select_dots-function.R
#' @include standard_error-function.R
#' @rdname summary_stats
#' @export
#'
summary_stats <-function(data, ...){
  UseMethod("summary_stats", data)
}


#' Summary stats for data.frame class
#'
#' @rdname summary_stats
#' @param meas dependent measure to compute summaries over
#' @param cols FALSE (default) for collapsing over all variables, or a vector of variables to collapses over in stages
#' @export
summary_stats.data.frame <-
  function(data, ...,meas,cols = FALSE) {
  assertthat::validate_that(
    is.data.frame(data) |
      tibble::is.tibble(data),
    assertthat::has_attr(data, "names")
  )
  assertthat::validate_that(!(missing(meas)))

if(is.character(cols))
{ data<-
  dplyr::group_by_(.data = data,
                            .dots = select_dots(cols))
data} else {
  data <-
    data
}

  standard_error <- function(x) {
    x <-
      stats::sd(x) / sqrt(length(x))
  }
  sd <- stats::sd

  data_out <-
    dplyr::summarise_at(data, c(paste0(meas)), .funs  = dplyr::funs(
      N = length,
      M = mean,
      SD = sd,
      SE = standard_error
    ))

  data_out

  }


#' A vectorized version of summary_stats.data.frame
#' @rdname summary_stats
#' @export
summary_stats.default <-
  Vectorize(
    FUN = function(data, meas, cols, ...)
      summary_stats.data.frame(data = data,
                               ...,
                               cols = cols,
                               meas = meas),
    vectorize.args = c("data", "meas"),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

