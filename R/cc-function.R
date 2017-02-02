#' Split a string at space (an alternative to \code{c()})
#'

#' \code{cc} is a wrapper around \code{\link[stringr]{str_split}} that takes a single quoted string whose elements are to be split at whitespace.
#'
#' @param string_to_split A single quoted string.
#' @param type One of \code{c} (default) to return a character vector; \code{i} to return an integer vector; \code{n} to return a numeric vector.
#' @return A character, integer, or numeric vector.
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
  function(string_to_split, type = "c") {
    string_to_split <-
      stringr::str_split(string_to_split, pattern = "[\\s]{1,3}", simplify = FALSE)
    string_to_split <-
      unlist(string_to_split)
    if (type == "c") {
      string_to_split <-
        as.character(string_to_split)
    } else if (type == "i") {
      string_to_split <-
        as.integer(string_to_split)
    } else if (type == "n") {
      string_to_split <-
        as.numeric(string_to_split)
    }
  }

