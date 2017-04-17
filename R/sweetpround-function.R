#' Sweeten a statistic: Helper for Stat Sweet
#' @importFrom magrittr %>%
#' @param .stat a numeric value representing a statistic
#' @param rnd_digit value to pass to round function, defaults to 2
#' @param interval (FALSE) Should P values be returned in a range?  APA suggests reporting p values in a range for figures and tables in some cases. Automatically returnd without leading zeros, so if lead_zero is ignored when interval=TRUE
#' @param lead_zero (FALSE) Should the leading zero (before the decimal) be preserved? Defaults to FALSE, removing the zero in accordance with APA recommendations.
#' @param ... (ignored)
#' @return value for passing to sweetstat
#' @family sweetstat functions
#' @include statround-function.R
#' @rdname sweetpround
#' @export
#'

sweetpround <-
  function(.stat,
           rnd_digit,
           interval = FALSE,
           lead_zero = FALSE,
           ...) {
    options(scipen = 999)

    assertthat::validate_that(is.numeric(.stat), .stat >= 0)
    p_dots <-
      pryr::named_dots(...)
    lapply(p_dots, eval, parent.frame())

    round_digit <-
      ifelse(missing(rnd_digit), 3L, as.integer(rnd_digit))

    stat <- .stat

    if (isTRUE(interval)) {
      stat_range = findInterval(stat, c(0, 0.001, 0.01, 0.05, 0.1, 0.99))

      codes = c("< .001",
                "< .01",
                "< .05",
                "< .10",
                "> .10")

      stat <-
        codes[stat_range]
      return(stat)
    } else {
      stat_out <-
        ifelse(is.na(stat),
               NA_character_, ifelse(
                 stat == "",
                 "",
                 ifelse(
                   0.0 <= stat & stat <= 0.0009999,
                   as.character("0.001"),
                   as.character(statround(stat, rnd_digit = round_digit))
                 )
               ))

      stat_out

    }

    if (isTRUE(lead_zero)) {
      return(stat_out)
    } else{
      stat_out <-
        ifelse(is.na(stat_out), "",
               ifelse(stat_out == "NA", "",
                      ifelse(
                        stat_out == "", "",
                        ifelse(stat_out == " ", "",
                               paste0(
                                 ".",
                                 strsplit(sub('0+$', '', stat_out),
                                          ".",
                                          fixed = TRUE)[[1]][[2]]
                               ))
                      )))

      stat_out <-
        ifelse(stat_out != "" &
                 nchar(strsplit(as.character(stat_out),
                                ".",
                                fixed = TRUE)[[1]][2]) == 1,
               paste0(stat_out, "0"),
               stat_out)

      return(stat_out)
    }
  }
