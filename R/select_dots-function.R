#' A convience function for programming with dplyr's select_
#' @importFrom magrittr %>%
#' @param data data to pass to \code{\link[dplyr]{select_}}'s .dots parameter.
#' @param  ... further arguments passed to or from other methods
#' @return a formula
#' @family SE dplyr convenience functions
#' @export
#'
select_dots <- function(data, ...) {
  UseMethod("select_dots", data)
}

#'
#' Pass a string to select_
#' @importFrom magrittr %>%
#' @rdname select_dots
#' @family SE dplyr convenience functions
#' #' @examples
#'
#' my_data <-
#'      data.frame(
#'           injection = c("y","n"),
#'           patient = c("DT","GWB"))
#'
#'   # pass a string
#'   dplyr::select_(mydata, .dots = select_dots('injection'))
#'
#'  y <-
#'      "injection"
#'
#'  mydata %>%
#'       select_(.dots = select_dots(paste0(y)))
#'
#'  # use helper functions
#'  mydata %>%
#'      select_(.dots = select_dots("contains(match = 'inject')"))
#'
#' # use helper functions
#' mydata %>%
#'     select_(
#' .dots = select_dots("contains(match = 'inject')")
#'             )
#'
#' # pass arguments as a list
#' mydata %>%
#' select_(.dots = select_dots(list("contains(match = 'inject'),
#' "patient")))
#'
#' @export

select_dots.default <-
  function(data, ...) {
    assertthat::is.string(data)

    paste("~", data, sep = "") %>%
      lapply(FUN = as.formula) %>%
      lazyeval::as_f_list(.) %>%
      lapply(stats::as.formula)
  }

#' Pass a character vector to select_
#' @importFrom magrittr %>%
#' @family SE dplyr convenience functions
#' @rdname select_dots
#' @export
select_dots.vector <-
  function(data, ...) {
    assertthat::validate_that(is.vector(data, mode = "character"))
    paste("~", data, sep = "") %>%
      lapply(stats::as.formula) %>%
      lazyeval::as_f_list(.) %>%
      lapply(stats::as.formula)
  }

#' Pass a list of column names to select_
#' @importFrom magrittr %>%
#' @family SE dplyr convenience functions
#' @rdname select_dots
#' @export
#'
select_dots.list <-
  function(data, ...) {
  assertthat::validate_that(is.list(data))
  lapply(data,
         function(x) {
           assertthat::validate_that(assertthat::is.string(x))
           x <- paste("~", x, sep = "")
         }) %>%
    lazyeval::as_f_list(.) %>%
    lapply(stats::as.formula)
}
