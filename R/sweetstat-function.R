#' APA-style decimal rounding for stats
#'
#' This is the generic version of the function.
#'
#' @importFrom magrittr %>%
#' @param .stat a numeric value representing a statistic
#' @param ... pass "rnd_digit" to set digits (defaults to 2)
#' @return stat as APA-rounded character vector
#' @family sweetstat functions
#' @include statround-function.R
#' @include sweetround-function.R
#' @rdname sweetstat
#' @export
#'
sweetstat <- function(.stat, ...) {
  UseMethod("sweetstat", .stat)
}


#' Pass a numeric  value to sweetstat
#' @rdname sweetstat
#' @export
sweetstat.numeric <- function(.stat, ...) {

  if (length(.stat) > 1) {
.stat <-
      sapply(.stat,
             MARGIN = 2,
             sweetstat,
             ... = ...)

  } else{
    .stat <-
      sweetround(.stat = .stat, ...)
 .stat
  }
}

#' Pass a numeric string value to sweetstat
#' @rdname sweetstat
#' @export
sweetstat.character <- function(.stat, ...) {
  if (length(.stat) > 1) {
    .stat <-
      sapply(as.numeric(.stat),
             MARGIN = 2,
             sweetstat,
             ...)
    .stat
  } else{
    .stat <-
      sweetround(.stat = as.numeric(.stat), ...)
    .stat
  }
}



#' Pass a integer  value to sweetstat
#' @rdname sweetstat
#' @export
sweetstat.integer <- function(.stat, ...) {
  .stat <-
    as.numeric(.stat)
  NextMethod("sweetstat")

}


#' Default method for sweetening a statistic
#' @rdname sweetstat
#' @export
sweetstat.default <- function(.stat, ...) {

if (is.list(.stat)) {
  if("data.frame" %in% attr(.stat, "class")){
    # with transfrom:

    .stat <-
      dplyr::mutate_at(.stat, .cols=names(which(sapply(.stat,is.double)==TRUE)),function(x)lapply(x,"sweetround"))
  } else {
    .stat <-
        lapply(as.numeric(.stat), sweetround,...)
    }
  } else {
    NextMethod("sweetstat")  }
}

