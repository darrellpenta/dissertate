#' Create quoted input for NSE \code{.dots}
#'

#' \code{fs_to_dots} takes a list of formulas or a character vector of such a list and "quotes" it for passing to the \code{.dots} argument of the NSE versions of dplyr \code{\link[dplyr]{nse}} functions like \code{\link[dplyr]{select_}}. Can be useful if the conditions for tidying and wrangling a dataset are specified in a list, for example.
#'
#' @param formlist Either a list of comma-separated RHS formulas, as \code{list(~f1, ~f2)}, or a character vector of such a list: \code{"list(~col1,col2)"}.
#' @return A quoted expression that can be interpred as a \code{dots} argument.
#' @examples
#'
#' # With a comma-separated list of RHS formulas
#'
#' filter_list <-
#'   flist_to_dots(list(~Sepal.Length < 5.0, ~Petal.Length > 1.3))
#'
#' head(
#' iris %>%
#'     dplyr::filter_(.dots = filter_list)
#'     )
#'
#' # With a character vector version of a 'list'
#'
#' df <-
#'   dplyr::tibble(
#'     col1 = 1:2,
#'     col2 = c("A","Z")
#'     dot_forms = c("list(~Sepal.Length,~Petal.Length)",
#'     "list(~Sepal.Length)"))
#'
#' select_form <-
#'   flist_to_dots(df$filter_form[2])
#' head(
#'   iris %>%
#'     dplyr::select_(.dots = select_form)
#'     )
#'
#' @export

fs_to_dots <-
  function(formlist) {
  if(!(is.character(formlist) | is.list(formlist))) { stop("Formlist should be either a list of comma-separated RHS formulas, as 'list(~f1, ~f2)', or a character vector of such a list: \"list(~col1,col2)\"")
  } else if(is.list(formlist)) {
    formlist <-
      formlist %>%
      lazyeval::as_f_list(.) %>%
      lapply(stats::as.formula)

    return(formlist)
  } else if(is.character(formlist)) {

    formlist <-
      paste0("~", formlist) %>%
      stats::as.formula(.) %>%
      lazyeval::f_eval(.) %>%
      lapply(lazyeval::uq) %>%
      unlist(recursive = TRUE) %>%
      lapply(function(x) paste0("~",x)) %>%
      lazyeval::as_f_list(.) %>%
      lapply(stats::as.formula)
    return(formlist)
  } else {
    stop()
  }
}

