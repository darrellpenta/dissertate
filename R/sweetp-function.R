#' APA-style decimal rounding for p-values
#'
#' This is the generic version of the function.
#'
#' @importFrom magrittr %>%
#' @inheritParams sweetpround
#' @return stat as APA-rounded character vector
#' @family sweetstat functions
#' @include statround-function.R
#' @include sweetpround-function.R
#' @rdname sweetp
#' @export
#'
sweetp <- function(.stat, ...) {
  UseMethod("sweetp", .stat)
}


#' Pass a numeric  value to sweetp
#' @rdname sweetp
#' @export
sweetp.numeric <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  if (length(.stat) > 1) {
    .stat <-
      sapply(.stat,
             MARGIN = 2,
             sweetpround,
             ... = ...)

  } else{

    .stat <-
      sweetpround(.stat = .stat,
                  ... = ...)
    .stat
  }
}

#' Pass a numeric string value to sweetp
#' @rdname sweetp
#' @export
sweetp.character <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  if (length(.stat) > 1) {
    .stat <-
      sapply(as.numeric(.stat),
             MARGIN = 2,
             sweetpround,
             ... = ...)

  } else{

    .stat <-
      sweetpround(.stat = as.numeric(.stat),
                  ... = ...)
    .stat
  }
}



#' Pass a integer  value to sweetp
#' @rdname sweetp
#' @export
sweetp.integer <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  .stat <-
    as.numeric(.stat)
  NextMethod("sweetp")

}


#' Default method for sweetening a statistic
#' @rdname sweetp
#' @export
sweetp.default <- function(.stat, ...) {
  p_dots <-  pryr::named_dots(...)
  lapply(p_dots, eval, parent.frame())

  if (is.list(.stat)) {
    if("data.frame" %in% attr(.stat, "class")){
      .stat <-
        dplyr::mutate_at(.stat, .cols=names(which(sapply(.stat,is.double)==TRUE)),function(x, ...){
          dots <-  pryr::named_dots(...)
          lapply(dots, eval, parent.frame())

          lapply(x,"sweetpround",...=...)})
    } else {
      .stat <-
        lapply(as.numeric(.stat), sweetpround,... = ... )
    }
  } else {
    NextMethod("sweetp")  }
}

