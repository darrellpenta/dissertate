#' APA-style decimal rounding for stats
#'
#' This is the generic version of the function.
#'
#' @importFrom magrittr %>%
#' @param .stat a numeric value representing a statistic
#' @param ... pass "rnd_digit" to set digits (defaults to 2)
#' @return stat as APA-rounded character vector
#' @family sweet_stat functions
#' @include statround-function.R
#' @include sweetround-function.R
#' @rdname sweet_stat
#' @export
#'
sweet_stat <- function(.stat, ...) {
  UseMethod("sweet_stat", .stat)
}


#' Pass a numeric  value to sweet_stat
#' @rdname sweet_stat
#' @export
sweet_stat.numeric <- function(.stat, ...) {

  if (length(.stat) > 1) {
.stat <-
      sapply(.stat,
             MARGIN = 2,
             sweet_stat,
             ... = ...)

  } else{
    .stat <-
      sweetround(.stat = .stat, ...)
 .stat
  }
}

#' Pass a numeric string value to sweet_stat
#' @rdname sweet_stat
#' @export
sweet_stat.character <- function(.stat, ...) {
  if (length(.stat) > 1) {
    .stat <-
      sapply(as.numeric(.stat),
             MARGIN = 2,
             sweet_stat,
             ...)
    .stat
  } else{
    .stat <-
      sweetround(.stat = as.numeric(.stat), ...)
    .stat
  }
}



#' Pass a integer  value to sweet_stat
#' @rdname sweet_stat
#' @export
sweet_stat.integer <- function(.stat, ...) {
  .stat <-
    as.numeric(.stat)
  NextMethod("sweet_stat")

}


#' Default method for sweetening a statistic
#' @rdname sweet_stat
#' @export
sweet_stat.default <- function(.stat, ...) {

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
    NextMethod("sweet_stat")  }
}

