#' Return a list of formulas or a character vector of such a list for the NSE dots arugment.
#'

#' \code{flist_to_dots} prepares a list of formulas or a character vector of such a list to be passed to the \code{.dots} argument of the NSE versions of \code{\link[dplyr]{index}} functions like \code{filter_}.
#'
#' @param formlist Either a list of comma-separated RHS formulas, as 'list(~f1, ~f2)', or a character vector of such a list: \\"list(~col1,col2)\\".
#' @return An expression that can be interpred as a \code{dots} argument.
#'
#' @export
#' @examples
#' # With a list of RHS formulas
#' filter_list <-
#'   list(~Sepal.Length < 5.0, ~Petal.Length > 1.3)
#' filter_list <-
#'   flist_to_dots(filter_list)
#' head(
#'   iris %>%
#'     dplyr::filter_(.dots = filter_list))
#'
#' #With a character vector of a list
#' df <-
#'   dplyr::tibble(
#'     col1 = 1:2,
#'     filter_form = c("list(~Sepal.Length,~Petal.Length)", "list(~Sepal.Length)")
#'   )
#'
#' select_form <-
#'   flist_to_dots(df$filter_form[2])
#' head(
#'   iris %>%
#'     dplyr::select_(.dots = select_form))}
#'

flist_to_dots <-
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
