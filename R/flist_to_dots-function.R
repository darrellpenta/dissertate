#' Return a list of formulas or a character vector of such a list for the NSE dots arugment.
#'
#' @param .formlist quoteed or unquoted list of comma-separated RHS formulas,
#' @return An expression that can be interpred as a \code{dots} argument.
#' @seealso sweet_dots
#' @export


flist_to_dots <-
  function(.formlist) {
    if(!(is.character(.formlist) | is.list(.formlist))) { stop("Formlist should be either a list of comma-separated RHS formulas, as 'list(~f1, ~f2)', or a character vector of such a list: \"list(~col1,col2)\"")
    } else if(is.list(.formlist)) {
      .formlist <-
         lazyeval::as_f_list(.formlist) %>%
        lapply(stats::as.formula)

      return(.formlist)
    } else if(is.character(.formlist)) {

      .formlist <-
        stats::as.formula(paste0("~", .formlist))
        .formlist <-
          lazyeval::f_eval(.formlist) %>%
        lapply(lazyeval::uq) %>%
        unlist(recursive = TRUE) %>%
        lapply(function(x) paste0("~",x))
      .formlist <-
        lazyeval::as_f_list(.formlist) %>%
        lapply(stats::as.formula)
      return(.formlist)
    } else {
      stop()
    }
  }


