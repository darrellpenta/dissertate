#' Return p-value as a range from a number without greater-than or less-than sign.
#'
#'
#' May be useful when numbers will be used in a LaTex document or places where only the numerical value as a character is wanted.
#'
#' @param p A number for which a p-value rage should be returned.
#' @return The bare significance range: e.g., "001"
#' @export

get_range.tex = function(p) {
  range = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("001" , "01", "05", "10", "10")
  codes[range]
}
options(scipen = 1)
