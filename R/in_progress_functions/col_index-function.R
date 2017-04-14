#' The names of a dataframe column in a numbered matrix
#'
#'
#' For convenience.
#'
#' @param X A dataframe.
#' @return A numbered matrix of columnames.
#' @rdname col_index
#' @export

col_index <- function(X) {
  matrix(names(X))
  }
