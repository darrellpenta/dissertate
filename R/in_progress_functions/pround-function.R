#' Helper function for sweetp
#'
#' @param p a numeric or character p-value
#' @return p as APA-rounded character vector
#' @family sweetstat functions
#' @rdname pround
#' @export
#'

pround <- function(p) {
  round(round(p / as.numeric(paste0(
    "0.000", substr(
      stringr::str_replace(p, "([\\.0]+)([1-9]+)", replacement = "\\2"),
      1,
      1
    )
  ))) * as.numeric(paste0(
    "0.000", substr(
      stringr::str_replace(p, "([\\.0]+)([1-9]+)",
                           replacement = "\\2"),
      1,
      1
    )
  )), digits = 3)
}
