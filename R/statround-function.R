#' Helper function for sweetstat
#'
#' @param .stat a number to be rounded
#' @param .rnd_digit number to pass to <code>round</code>; defaults to 3
#' @param ... (ignored) args to be passed to other methods
#' @return a rounded numeric
#' @family sweetstat functions
#' @rdname statround
#' @export

statround <- function(.stat, ...) {
  UseMethod("statround", .stat)
}


#' Pass a numeric value to statround
#' @rdname statround
#' @export
statround.numeric <- function(.stat, ..., .rnd_digit = 3) {
  assertthat::validate_that(is.character(.stat))

  rnd_digit <-
    .rnd_digit
  stat_out <-
    ifelse(is.na(.stat), NA, round(round(.stat / as.numeric(paste0(
      "0.000",
      substr(
        stringr::str_replace(.stat, "([\\.0]+)([1-9]+)",
                             replacement = "\\2"),
        1,
        1
      )
    ))) * as.numeric(paste0(
      "0.000",
      substr(
        stringr::str_replace(.stat, "([\\.0]+)([1-9]+)",
                             replacement = "\\2"),
        1,
        1
      )
    )), digits = rnd_digit))
  stat_out
}





#' Pass a numeric string to statround
#' @rdname statround
#' @export
statround.character <- function(.stat, ..., .rnd_digit = 3) {
      assertthat::validate_that(is.character(.stat))

   rnd_digit <-
        .rnd_digit
    stat_out <-
ifelse(is.na(.stat), NA, round(round(as.numeric(.stat) / as.numeric(paste0(
        "0.000",
        substr(
          stringr::str_replace(as.numeric(.stat), "([\\.0]+)([1-9]+)",
                               replacement = "\\2"),
          1,
          1
        )
      ))) * as.numeric(paste0(
        "0.000",
        substr(
          stringr::str_replace(.stat, "([\\.0]+)([1-9]+)",
                               replacement = "\\2"),
          1,
          1
        )
      )), digits = rnd_digit))
stat_out
    }



#' Vectorized statround
#' @rdname statround
#' @export
statround.default <-
  Vectorize(
    function(.stat, ...)
      statround(.stat = .stat, ... = ...),
    vectorize.args = c(".stat"),
    SIMPLIFY = FALSE
  )


