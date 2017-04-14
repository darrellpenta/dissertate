#' APA-style decimal rounding for p-vals
#'
#' This is the generic version of the function.
#'
#' @importFrom magrittr %>%
#' @param stat a statistic to be rounded in APA style
#' @return p value stat as APA-rounded character vector
#' @family sweetstat functions
#' @rdname sweetp
#' @examples
#' lapply(runif(10,0.0004,0.034), sweetp)
#'
#' @export
#'
sweetp<- function(stat, not_as_range, snip_lead_zero) {
  UseMethod("sweetp", stat)
}



#' A vectorized vesion of the \code{sweetp} function
#' @param not_as_range P values be returned in a range? (APA suggests a range for figures and tables in some cases)
#' @param snip_lead_zero Should the leading zero (before the decimal) be remved? APA recommends this for numbers that cannot exceed 1.
#' @family sweetstat functions
#' @rdname sweetp
#' @include pround-function.R
#' @export
#'
sweetp.default <-
  Vectorize(
    FUN = function(stat)
      sweetp(stat, not_as_range, snip_lead_zero),
    vectorize.args = c("stat"),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

#' Sweeten an numeric vector
#'

#'
#' @family sweetstat functions
#' @rdname sweetp
#' @include pround-function.R
#' @export
#'
sweetp.numeric <- function(stat,
                   not_as_range,
                   snip_lead_zero) {
  options(scipen = 999)

  if (missing(not_as_range)) {
    not_as_range = TRUE
  }

  if (missing(snip_lead_zero)) {
    snip_lead_zero = TRUE
  }

  if (!(isTRUE(not_as_range))) {
    range = findInterval(stat, c(0, 0.001, 0.01, 0.05, 0.1, 0.99))
    codes = c("< .001",
              "< .01",
              "< .05",
              "< .10",
              "> .10")
    codes[range]
    range
  } else {
    stat <-
      ifelse(
        is.na(stat),
        "NA",
        ifelse(
          0.0 <= stat & stat <= 0.0009999,
          as.character("0.001"),
          as.character(pround(p = stat))
        )
      )

    stat <-
      ifelse (
        isTRUE(snip_lead_zero &
                 stat != "NA"),
        paste0(".", strsplit(sub('0+$', '', stat), ".", fixed = TRUE)[[1]][[2]]),
        ifelse(isTRUE(snip_lead_zero) &
                 stat == "NA", "", stat)
      )
    stat <-
      ifelse(stat != "" &
               nchar(strsplit(as.character(stat), ".", fixed = TRUE)[[1]][2]) == 1, paste0(stat, "0"), stat)

  }
  stat
}

#' Sweeten a characer vector
#'
#' @family sweetstat functions
#' @rdname sweetp
#' @include pround-function.R
#' @export
#'
 sweetp.character = function(stat,
                           not_as_range,
                           snip_lead_zero) {
 options(scipen = 999)

   stat <-
            as.numeric(stat)

   if (missing(not_as_range)) {
     not_as_range = TRUE
   }

   if (missing(snip_lead_zero)) {
     snip_lead_zero = TRUE
   }


   if (!(isTRUE(not_as_range))) {
     range = findInterval(stat, c(0, 0.001, 0.01, 0.05, 0.1, 0.99))
     codes = c("< .001",
               "< .01",
               "< .05",
               "< .10",
               "> .10")
     codes[range]
     range
   } else {
     stat <-
       ifelse(
         is.na(stat),
         "NA",
         ifelse(
           0.0 <= stat & stat <= 0.0009999,
           as.character("0.001"),
           as.character(pround(p = stat))
         )
       )

     stat <-
       ifelse (
         isTRUE(snip_lead_zero &
                  stat != "NA"),
         paste0(".", strsplit(sub('0+$', '', stat), ".", fixed = TRUE)[[1]][[2]]),
         ifelse(isTRUE(snip_lead_zero) &
                  stat == "NA", "", stat)
       )
     stat <-
       ifelse(stat != "" &
                nchar(strsplit(as.character(stat), ".", fixed = TRUE)[[1]][2]) == 1, paste0(stat, "0"), stat)

   }
   stat
 }


