#' A vectorized version of \code{utils::combn}
#'
#' @inheritParams utils::combn
#' @family factorial-design functions
#' @export
combine_factor_levels <-
  Vectorize(function(x, m, FUNV = NULL, simplify, byrow)
  utils::combn(x, m, FUN = FUNV, simplify, byrow),
  vectorize.args = c("x","m"))
