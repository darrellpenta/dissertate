#' Compute a standard error statistic for a vector

#' @param x a vector of numeric values
#' @return a standard error
#' @rdname standard_error
#' @export
#'


standard_error <- function(x){

    x <-
      stats::sd(x) / sqrt(length(x))

}
