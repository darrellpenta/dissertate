#' Compute the standard error of \code{x}.
#'
#'
#' Standard error.
#'
#' @param x A numeric vector (such as the column of a dataframe).
#' @return The standard error of \code{x}
#' @export


se <- function(x) {
  sd(x) / sqrt(length(x))
}
