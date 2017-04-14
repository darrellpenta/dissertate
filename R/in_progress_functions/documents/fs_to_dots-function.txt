#' Create quoted input for NSE \code{.dots}
#'

#' \code{fs_to_dots} takes a list of formulas or a character vector of such a list and "quotes" it for passing to the \code{.dots} argument of the NSE versions of dplyr \code{\link[dplyr]{nse}} functions like \code{\link[dplyr]{select_}}. This can be useful if the conditions for tidying and wrangling a dataset are specified in a separate list, for example.
#'
#' @param formlist Either a list of comma-separated RHS formulas, as \code{list(~f1, ~f2)}, or a character vector of such a list: \code{"list(~col1,col2)"}. Use single quotation marks to refer to named conditions: \code{list(~col1 == 'var1')}. For multiple conditions connected with explicit logical operators (e.g., \code{|,||, &, &&}), put the entire string in a single formula: \code{list(~col1 > 2 | col1 < 3, ~col2 != 'var1')}
#' @return A quoted expression that can be interpred as a \code{dots} argument.
#' @examples
#'
#' # With a comma-separated list of RHS formulas
#' dot_list <-
#'   fs_to_dots(list( ~ Sepal.Length < 5.0 | Sepal.Length < 2.0,
#'                    ~ Species!= 'versicolor'))
#'
#' head(
#'   iris %>%
#'     dplyr::filter_(.dots = dot_list)
#' )
#'
#'
#' # With a character vector version of a 'list'
#'
#' df <-
#'   dplyr::tibble(
#'     dot_forms = c("list(~Sepal.Length,~Petal.Length)",
#'                   "list(~Species == 'virginica'| Species == 'versicolor#')"
#'                   )
#'   )
#'
#' iris %>%
#'   dplyr::select_(.dots =
#'                    fs_to_dots(df$dot_forms[1])) %>%
#'   stats::lm(.[,1] ~ .[,2],
#'      data = .)
#'
#' iris %>%
#'   dplyr::filter_(.dots =
#'                    fs_to_dots(df$dot_forms[2])) %>%
#'   stats::aov(Sepal.Length ~ Petal.Length + Error(Species/Petal.Length),
#'       data = .)
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
      lazyeval::as_f_list(.)  %>%
      lapply(stats::as.formula)
    return(formlist)
  } else {
    stop()
  }
}



