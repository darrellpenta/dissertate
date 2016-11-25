#' The names of a dataframe column in a numbered matrix.
#'
#'
#' For convenience.
#'
#' @param df A dataframe.
#' @return A numbered matrix of columnames.
#' @export

col_index <- function(X) {
  matrix(names(X))}
