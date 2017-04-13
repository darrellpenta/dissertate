#' APA-style decimal rounding for stats
#'
#' This is the generic version of the function.
#'
#' @importFrom magrittr %>%
#' @param stat a statistic to be rounded in APA style
#' @param ... further arguments passed to or from other methods
#' @return stat as APA-rounded character vector
#' @family sweetstat functions
#' @include statround-function.R
#' @rdname sweetstat
#' @export
#'
sweetstat<- function(stat, ...) {
  UseMethod("sweetstat", data)
}

#' A vectorized vesion of the \code{sweetstat} function
#' @family sweetstat functions
#' @include statround-function.R
#' @rdname sweetstat
#' @export
sweetstat.default <-
  Vectorize(
    FUN = function(stat)
      sweetstat(stat),
    vectorize.args = c("stat"),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

#' Sweeten a stat for APA-styling
#'
#' @family sweetstat functions
#' @include statround-function.R
#' @rdname sweetstat
#' @export

sweetstat.numeric <- function(stat) {
  options(scipen = 999)

  stat <-
    ifelse(is.na(stat),
           NA,
           ifelse(
             0.0 <= stat & stat <= 0.0000999,
             0.0001,
             ifelse(
               0.0001 <= stat & stat <= 0.0009999,
               0.001,
               ifelse (
                 0.001 <= stat & stat <= 0.9999999,
                 statround(s = stat),
                 ifelse(1.0 <= stat &
                          stat <=  99999.99999, round(stat, 2), 99999)
               )
             )
           ))

  stat <-
    ifelse(stat < 1, round(stat, 2), stat)
  stat <-
    ifelse(stringr::str_detect(as.character(stat), pattern = "\\."),
           stat,
           paste0(stat, ".00"))
  stat <-
    ifelse(nchar(strsplit(as.character(stat), ".", fixed = TRUE)[[1]][2]) == 1,
           paste0(stat, "0"),
           ifelse(nchar(
             strsplit(as.character(stat), ".", fixed = TRUE)[[1]][2]
           ) < 1, paste0(stat, "00"), paste(stat)))

}

#' @family sweetstat functions
#' @include statround-function.R
#' @rdname sweetstat
#' @export
#'


sweetstat.character <- function(stat) {
  options(scipen = 999)
  stat<-
    as.numeric(stat)

  stat <-
    ifelse(is.na(stat),
           NA,
           ifelse(
             0.0 <= stat & stat <= 0.0000999,
             0.0001,
             ifelse(
               0.0001 <= stat & stat <= 0.0009999,
               0.001,
               ifelse (
                 0.001 <= stat & stat <= 0.9999999,
                 statround(s = stat),
                 ifelse(1.0 <= stat &
                          stat <=  99999.99999, round(stat, 2), 99999)
               )
             )
           ))

  stat <-
    ifelse(stat < 1, round(stat, 2), stat)
  stat <-
    ifelse(stringr::str_detect(as.character(stat), pattern = "\\."),
           stat,
           paste0(stat, ".00"))
  stat <-
    ifelse(nchar(strsplit(as.character(stat), ".", fixed = TRUE)[[1]][2]) == 1,
           paste0(stat, "0"),
           ifelse(nchar(
             strsplit(as.character(stat), ".", fixed = TRUE)[[1]][2]
           ) < 1, paste0(stat, "00"), paste(stat)))

}






