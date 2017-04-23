#' Create dots
#'
#' The function takes a string, a vector, a formula, a list of formulas, or a string representing such a list and "quotes" it for passing to the dplyr functions.
#'
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @param .data data to pass to selects "dots" parameter
#' @param ... optional arguments passed to/from other methods
#' @return a formula
#' @rdname sweet_dots
#' @include dots_maker-function.R
#' @family SE dplyr convenience functions
#' @examples
#'
#' # Pass a string
#'  dplyr::select_(iris, .dots = sweet_dots("Sepal.Length"))
#'  dplyr::select_(iris, .dots = sweet_dots("dplyr::matches('Sepal')"))
#'  dplyr::filter_(iris, .dots = sweet_dots("Sepal.Length == 5.1 & Species != 'setosa'"))
#'
#' # Pass a vector
#' dplyr::select_(iris, .dots = sweet_dots(c("Sepal.Length","Species")))
#' @export
sweet_dots <- function(.data, ...) {
  UseMethod("sweet_dots", .data)
}


#' @rdname sweet_dots
#' @export
sweet_dots.character <- function(.data, ...) {
  assertthat::validate_that(is.character(.data))
  s_dots <-
    pryr::named_dots(...)
  lapply(s_dots, eval, parent.frame())

  if (length(.data) > 1) {
    .data <-
      sapply(.data,
             MARGIN = 2,
             sweet_dots,
             ... = ...)
    .data
  } else{
    .data <-
      dots_maker(.data = .data, ... = ...)
    .data
  }
}

#' @rdname sweet_dots
#' @export
sweet_dots.integer <- function(.data, ...) {
  assertthat::validate_that(is.integer(.data))
  s_dots <-
    pryr::named_dots(...)
  lapply(s_dots, eval, parent.frame())

  if (length(.data) > 1) {
    .data <-
      sapply(.data,
             MARGIN = 2,
             sweet_dots,
             ... = ...)
    .data
  } else{
    .data <-
      dots_maker(.data = .data, ... = ...)
    .data
  }
}



#' @rdname sweet_dots
#' @export
sweet_dots.formula <-
  function(.data, ...) {
    assertthat::validate_that(length(.data) == 2)
    {
      .data <-
        .data
    }
  }


#' @rdname sweet_dots
#' @export

sweet_dots.default <-
  function(.data, ...) {
    assertthat::validate_that(!("data.frame" %in% attr(.data, "class")))
    s_dots <-
      pryr::named_dots(...)
    lapply(s_dots, eval, parent.frame())

    if (is.list(.data)) {
      if (any(sapply(.data, lazyeval::is_formula)) &
          all(sapply(.data, lazyeval::is_formula))) {
        .data <-
          lazyeval::as_f_list(.data) %>%
          lapply(stats::as.formula)
      } else if (any(sapply(.data, is.character) |
                     any(sapply(.data, is.integer)) |
                     any(sapply(.data, lazyeval::is_formula)))) {
        .data <-
          lapply(.data, sweet_dots)
      } else {
        stop()
      }
    } else {
      NextMethod("sweet_dots")
    }
  }
