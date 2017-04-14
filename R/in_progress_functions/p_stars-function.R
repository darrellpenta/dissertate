#' Star-type signigicance symbols
#' @param p value whose significance is to be indicated with a star
#' @param show_ns should non-significant values be shown with "n.s."? Default is TRUE
#' @return text stars
#' @include standard_error-function.R
#' @rdname p_stars
#' @export
#'
p_stars <-
  function(p, show_ns = TRUE) {
    stars = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
    codes = c("***" , "**", "*", ".", ifelse(isTRUE(show_ns), "n.s.", ""))
    codes[stars]
  }
