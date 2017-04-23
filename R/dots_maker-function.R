#' Helper functions for sweet_dot
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @param .data data to generate dots from
#' @param ... optional arguments passed to/from other methods (ignore)
#' @return a formula
#' @rdname dots_maker
#' @family SE dplyr convenience functions
#' @export
#'
dots_maker <-
  function(.data, ... ){
    dm_dots <-
      pryr::named_dots(...)
    lapply(dm_dots, eval, parent.frame())
    data_out <-
      paste("~", .data, sep = "") %>%
      lapply(stats::as.formula)
    data_out <-
      lazyeval::as_f_list(data_out) %>%
      lapply(stats::as.formula)
    data_out
  }
