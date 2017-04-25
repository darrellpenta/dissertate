#' Sort a data frame by column
#'
#' Creates a table of aov summary
#' @param x a data frame
#' @param decreasing Decreasing oder?
#' @param by index to sort on
#' @param ... further arguments passed to or from other methods (ignored)
#' @return a sorted data frame
#' @rdname sort.data.frame
#' @examples
#' ##sort(iris, by="Sepal.Length")
#' ##sort(iris, by=c("Species","Sepal.Length"))
#' ##sort(iris, by=1:2)
#' ##sort(iris, by="Sepal.Length",decreasing=TRUE)

#' @export
sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}
