#' Sweeten a statistic: Helper for Stat Sweet
#' @importFrom magrittr %>%
#' @param .stat a numeric value representing a statistic
#' @param rnd_digit value to pass to round function, defaults to 2
#' @param interval  Which .stat should return is value as an interval cut-off?  (Default is \code{none}, so a .stat is returned representing the underlying number); \code{sig} will return intervals for values in the range: 0<>0.001<>.01<>.05, and otherwise return a sweet p; code will return the interval values in the range: 0<>0.001<>.01<>.05<>.10<>99. APA suggests reporting p values in a range for figures and tables in some cases. Automatically returned without leading zeros, so lead_zero is ignored when interval=TRUE
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
           interval ="sig",
           lead_zero = FALSE,
           ...) {
    options(scipen = 999)
    
statout <- function(.stat, rnd_digit=2) {
  round_digit <-
        ifelse(missing(rnd_digit), 3L, as.integer(rnd_digit))
      
      .stat <- ifelse(is.na(.stat),
                      NA_character_,
                      ifelse(.stat == "",
                             "", ifelse(
                               .stat > .99,
                               as.character("1.00"),
                               ifelse(
                                 0.0 <= .stat & .stat <= 0.0009999,
                                 as.character("0.001"),
                                 as.character(statround(.stat, rnd_digit = round_digit))
                               )
                             )))
      
      return(.stat)
    }
trimlead <- function(.stat) {
      .stat <-
        ifelse(is.na(.stat), "",
               ifelse(.stat == "NA", "",
                      ifelse(
                        .stat == "", "",
                        ifelse(.stat == " ", "",
                               paste0(
                                 ".",
                                 strsplit(sub('0+$', '', .stat),
                                          ".",
                                          fixed = TRUE)[[1]][[2]]
                               ))
                      )))
      
      .stat <-
        ifelse(.stat != "" &
                 nchar(strsplit(as.character(.stat),
                                ".",
                                fixed = TRUE)[[1]][2]) == 1,
               paste0(.stat, "0"),
               .stat)
      
      return(.stat)
    }
all_interval <- function(.stat) {
      stat_range = findInterval(.stat, c(0, 0.001, 0.01, 0.05, 0.1, 99))
      
      codes = c("< .001",
                "< .01",
                "< .05",
                "< .10",
                "> .10")
      
      .stat <-
        codes[stat_range]
      return(.stat)
    }
sig_interval <- function(.stat, rnd=3) {
  
  stat_range <- findInterval(.stat, c(0, 0.001, 0.01, 0.05,99))
  
  codes <- c("$\\textless$ .001",
             "$\\textless$ .01",
             "$\\textless$ .05",
             as.character(paste0("$=$ ",trimlead(statout(.stat)))))
  .stat <-
    codes[stat_range]
  return(.stat)
  
}


if (!is.na(.stat))
      assertthat::validate_that(is.numeric(.stat), .stat >= 0)  
stat <-
      .stat
    
    if (identical(interval, "all")) {
      stat <-
        all_interval(.stat = stat)
      return(stat)
    } else {
      stat <-
        sig_interval(.stat = stat, ... = ...)
      return(stat)
    }
    # } else {
    #   stat <-
    #     statout(value = stat, ... = ...)
    #   if (isTRUE(lead_zero)) {
    #     return(stat)
    #   } else {
    #     stat <-
    #       trimlead(stat)
    #     return(stat)
    #   }
    # }
  }
