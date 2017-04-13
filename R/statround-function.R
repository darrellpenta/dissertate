#' Helper function for sweetstat
#' @param s a number to be rounded
#' @param ... further arguments passed to or from other methods
#' @return a rounded numeric
#' @family sweetstat functions
#' @rdname statround
#' @export

statround <- function(s) {
  round(round(s / as.numeric(paste0(
    "0.000", substr(
      stringr::str_replace(s, "([\\.0]+)([1-9]+)", replacement = "\\2"),
      1,
      1
    )
  ))) * as.numeric(paste0(
    "0.000", substr(
      stringr::str_replace(s, "([\\.0]+)([1-9]+)",
                           replacement = "\\2"),
      1,
      1
    )
  )), digits = 3)
}
