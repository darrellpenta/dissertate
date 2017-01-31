#' Split a string at space (an alternative to \code{c()})
#'

#' \code{cc} is a wrapper around \code{\link[stringr]{str_split}} that takes a single quoted string whose elements are to be split at whitespace.
#'
#' @param string_to_split A single quoted string.
#' @return A simplified character matrix
#' @examples
#'
#' # compare: c("A","B","C") vs. cc("A B C")
#' c1 <-
#'   c("A","B","C")
#' c2 <-
#'    cc("A B")
#'
#' c1 == c2
#'
#' @export


cc <-
  function(string_to_split){
    string_to_split <-
      stringr::str_split(string_to_split,pattern = "[\\s]{1,3}",simplify = TRUE)
  }
