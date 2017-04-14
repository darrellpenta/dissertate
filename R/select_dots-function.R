#' A convience function for programming with dplyr's select_
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @param data data to pass to selects "dots" parameter
#' @return a formula
#' @rdname select_dots
#' @family SE dplyr convenience functions
#' @export
#'
select_dots <- function(data) {
  UseMethod("select_dots", data)
}


#' Pass a string to select
#'
#' @rdname select_dots

#' @export

select_dots.default <-
  function(data) {
    assertthat::is.string(data)

    data_out <-
    paste("~", data, sep = "") %>%
      lapply(FUN = stats::as.formula)
    data_out<-
      lazyeval::as_f_list(data_out) %>%
      lapply(stats::as.formula)
    data_out
  }

#' Pass a character vector to select_

#' @rdname select_dots
#' @export
#'
select_dots.vector <-
  function(data) {
    assertthat::validate_that(is.vector(data, mode = "character"))
    data_out <-
    paste("~", data, sep = "") %>%
      lapply(stats::as.formula)
    data_out <-
      lazyeval::as_f_list(data_out) %>%
      lapply(stats::as.formula)
    data_out
  }

#' Pass a list of column names to select_
#'
#' @rdname select_dots
#' @export
#'
select_dots.list <-
  function(data) {
  assertthat::validate_that(is.list(data))
  data_out<-
    lapply(data,
         function(x) {
           assertthat::validate_that(assertthat::is.string(x))
           x <- paste("~", x, sep = "")
         })
  data_out <-
    lazyeval::as_f_list(data_out) %>%
    lapply(stats::as.formula)
}
