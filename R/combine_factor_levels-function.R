#' A vectorized version ofthe combn function
#'
#' @inheritParams utils::combn
#' @inheritParams base::Vectorize
#' @param FUNV a null function
#' @param byrow passed to matrix function
#' @family factorial-design functions
#' @rdname combine_factor_levels
#' @export
combine_factor_levels <-
  Vectorize(function(x, m, FUNV = NULL, simplify, byrow)
  utils::combn(x, m, FUN = FUNV, simplify, byrow),
  vectorize.args = c("x","m"))
