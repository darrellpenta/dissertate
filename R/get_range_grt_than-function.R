#' Return p-value as a range with in indicator for values greater than .10.
#'
#'
#' May be useful when non-singificant values are going to be displayed or reported.
#'
#' @param p A number for which a p-value rage should be returned.
#' @param print_marg Should marginally significant values be printed? Set to FALSE
#' @return The significance range: e.g., "<.001"
#' @export

get_range_grt_than = function(p, print_marg = FALSE) {
  range = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("<.001" ,
            "<.01",
            "<.05",
            ifelse(isTRUE(print_marg),paste0("=",round(p,3)),"<.10"), ">.10")
  codes[range]
}
options(scipen = 1)
