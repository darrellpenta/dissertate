#' A wrapper for \code{\link[sjmisc]{get_label}} suitable for use with \code{ggplot2}'s \code{xlab} function.
#'

#' For dataframes with labels (see \code{\link[sjmisc]{get_label}}),  \code{labelled_x} returns an epxression (for LaTex = TRUE) or string that can be passed to \code{xlab} in a \code{ggplot} (see \code{\link[ggplot2]{ggtitle}}).
#'
#' @param xdata A dataframe with labelled columns.
#' @param xname The (bare) name of the column to be used as the x label.
#' @param tex Logical (default = TRUE); Does the label contain LaTeX?
#' @return If tex = TRUE, the column label as a \code{\link[grDevices]{plotmath}} expression (see \code{\link[latex2exp]{TeX}} for details; or a character string.
#' @export



labelled_x = function(xdata, xname, tex = TRUE) {
  xlb =  lazyeval::f_eval(~ dplyr::select_(.data = xdata, xname))
  xlb = as.character(get_label(xlb))
  if(isTRUE(tex)){xlb = latex2exp::TeX(xlb)} else{xlb}

}
