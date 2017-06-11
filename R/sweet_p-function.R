#' APA-style decimal rounding for p-values
#'
#' This is the generic version of the function.
#'
#' @importFrom magrittr %>%
#' @inheritParams sweetpround
#' @return stat as APA-rounded character vector
#' @family sweet_stat functions
#' @include statround-function.R
#' @include sweetpround-function.R
#' @rdname sweet_p
#' @export
#'
sweet_p <- function(.stat, ...) {
  UseMethod("sweet_p", .stat)
}


#' Pass a numeric  value to sweet_p
#' @rdname sweet_p
#' @export
sweet_p.numeric <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  if (length(.stat) > 1) {
    .stat <-
      sapply(.stat,
             sweetpround)

  } else{

    .stat <-
      sweetpround(.stat = .stat)
    .stat
  }
}

#' Pass a numeric string value to sweet_p
#' @rdname sweet_p
#' @export
sweet_p.character <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  if (length(.stat) > 1) {
    .stat <-
      sapply(as.numeric(.stat),
             sweetpround)

  } else{

    .stat <-
      sweetpround(.stat = as.numeric(.stat))
    .stat
  }
}



#' Pass a integer  value to sweet_p
#' @rdname sweet_p
#' @export
sweet_p.integer <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  .stat <-
    as.numeric(.stat)
  NextMethod("sweet_p")

}


#' Default method for sweetening a statistic
#' @rdname sweet_p
#' @export
sweet_p.default <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  if (is.list(.stat)) {
    if("data.frame" %in% attr(.stat, "class")){
      .stat <-
        dplyr::mutate_at(.stat, .cols=names(which(sapply(.stat,is.double)==TRUE)),function(x, ...){
          dots <-  pryr::named_dots(...)
          lapply(dots, eval, parent.frame())

          lapply(x,"sweetpround")})
    } else {
      .stat <-
        lapply(as.numeric(.stat), sweetpround.numeric)
    }
  } else {
    NextMethod("sweet_p")  }
}

