#' Return p-value as a range from a number.
#'
#'
#' For when p-value stars are not appropriate, etc.
#'
#' @param p A number for which a p-value rage should be returned.
#' @return The significance range: e.g., "<.001"
#' @export

get_range = function(p) {
  range = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("<.001" , "<.01", "<.05", "<.10", " ")
  codes[range]
}
options(scipen = 1)
