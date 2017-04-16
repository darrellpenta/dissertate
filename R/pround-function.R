#' pround: Helper function for sweetstat
#'
#' @param .p a number to be rounded
#' @param .rnd_digit number to pass to <code>round</code>; defaults to 3
#' @param ... (ignored) args to be passed to other methods
#' @return a rounded numeric p value
#' @family sweetstat functions
#' @rdname pround
#' @export

pround <- function(.p, ...) {
  UseMethod("pround", .p)
}


#' Pass a numeric value to pround
#' @rdname pround
#' @export
pround.numeric <- function(.p, ..., .rnd_digit = 3) {
  assertthat::validate_that(is.numeric(.p))
  assertthat::validate_that(is.numeric(.rnd_digit))

  rnd_digit <-
    .rnd_digit
  p_out <-
    ifelse(is.na(.p), NA, round(round(.p / as.numeric(paste0(
      "0.000", substr(
        stringr::str_replace(.p, "([\\.0]+)([1-9]+)", replacement = "\\2"),
        1,
        1
      )
    ))) * as.numeric(paste0(
      "0.000", substr(
        stringr::str_replace(.p, "([\\.0]+)([1-9]+)",
                             replacement = "\\2"),
        1,
        1
      )
    )), digits = rnd_digit))
  p_out
}


#' Pass a numeric string to pround
#' @rdname pround
#' @export
pround.character <- function(.p, ..., .rnd_digit = 3) {
  assertthat::validate_that(is.character(.p))
  assertthat::validate_that(is.numeric(.rnd_digit))

  rnd_digit <-
    .rnd_digit
  p_out <-
    ifelse(is.na(.p), NA, round(round(as.numeric(.p) / as.numeric(paste0(
      "0.000", substr(
        stringr::str_replace(as.numeric(.p), "([\\.0]+)([1-9]+)", replacement = "\\2"),
        1,
        1
      )
    ))) * as.numeric(paste0(
      "0.000", substr(
        stringr::str_replace(as.numeric(.p), "([\\.0]+)([1-9]+)",
                             replacement = "\\2"),
        1,
        1
      )
    )), digits = rnd_digit))
  p_out
}



#' Vectorized pround
#' @rdname pround
#' @export
pround.default <-
  Vectorize(
    function(.p, ...)
      pround(.p = .p, ... = ...),
    vectorize.args = c(".p"),
    SIMPLIFY = FALSE
  )


