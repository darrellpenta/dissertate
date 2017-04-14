#' Standard summary stats
#'
#' This is the generic version of the \code{summary_stats}
#'
#' @importFrom magrittr %>%
#' @param .data data for which a summary is to be computed
#' @param .cols a character vector of names to be included in summaries
#' @param .variable variable over which summary is to be computed
#' @param ... additional arguments
#' @return a data frame with columns for:  N = length, M = mean, SD = sd, SE = standard_error
#' @family aov functions
#' @include select_dots-function.R
#' @include standard_error-function.R
#' @rdname summary_stats
#' @export
#'

summary_stats <- function(.data, .cols, .variable, ...) {
  assertthat::validate_that(
    is.data.frame(.data) |
      tibble::is.tibble(.data),
    assertthat::has_attr(.data, "names")
  )

  assertthat::validate_that(!(missing(.variable)))
  assertthat::validate_that(is.vector(.variable, mode = "character"))

  variable_ <-
    .variable

  if(missing(.cols)){
    data_ <-
      .data
    data_
  } else {
    assertthat::validate_that(is.vector(.cols, mode = "character"))
    cols_ <-
      .cols
    data_ <-
      .data %>%
      dplyr::group_by_(.dots = select_dots(cols_))
    data_
  }




  standard_error <- function(x) {
    x <-
      stats::sd(x) / sqrt(length(x))
  }
 sd <- stats::sd

  data_ <-
    data_ %>%
    dplyr::summarise_at(c(paste0(variable_)), .funs  = dplyr::funs(
        N = length,
        M = mean,
        SD = sd,
        SE = standard_error
      ))

}

summary_stat <-
  Vectorize(
    FUN = function(.data, .cols, .variable, ...)
      summary_stat(.data, .cols, .variable, ...),
    vectorize.args = c(".data", ".variable"),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
