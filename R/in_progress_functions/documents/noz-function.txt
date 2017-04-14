#' Remove leading zeros from a number.
#'
#' Useful when numbers are going to be used in APA-foramatted documents. Note that this concerts the number to a character vector and is not guaranteed to produce the appropriate result in all cases.
#'
#' @param x A numeric value.
#' @return \code{x} as a character with leading zeroes removed
#' @export

noz = function(x) {
  gsub("0\\.", "\\.", x)
}
