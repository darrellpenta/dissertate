#' Return p-value significance star(s) from a number.
#'
#'
#' Uses the standard range of intervals to determine the nubmer of stars.
#'
#' @param p A number for which a p-value star should be returned.
#' @return A character star: e.g., ***,**,*, ., NULL
#' @export


get_stars = function(p) {
  stars = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("***" , "**", "*", ".", " ")
  codes[stars]
}
options(scipen = 1)
