#' Sweeten a statistic: Helper for Stat Sweet
#' @importFrom magrittr %>%
#' @param .stat a numeric value representing a statistic
#' @param rnd_digit value to pass to round function, defaults to 2
#' @param ... (ignored)
#' @return value for passing to sweetstat
#' @family sweetstat functions
#' @include statround-function.R
#' @rdname sweetround
#' @export
#'

sweetround <- function(.stat, rnd_digit, ...){
options(scipen = 999)
assertthat::validate_that(is.numeric(.stat))

round_digit <-
  ifelse(missing(rnd_digit), 2L, as.integer(rnd_digit))

stat <- .stat

negative_stat <-
  ifelse(!(is.na(stat)) && stat < 0, TRUE, FALSE)

stat <-
  ifelse(isTRUE(negative_stat), stat * -1, stat)

stat_out <-
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
               statround(.stat = stat),
               ifelse(1.0 <= stat & stat <=  99999.99999,
                      round(stat, digits = round_digit),
                      as.numeric(
                        paste(
                          stringr::str_split(stat,"[[:punct:]]")[[1]][1],
                          stringr::str_split(as.character(round(as.numeric(paste0("0.",stringr::str_split(stat,"[[:punct:]]")[[1]][2])), digits = round_digit)),"[[:punct:]]")[[1]][2], sep = ".")))))))


stat_out <-
  ifelse(is.na(stat_out), NA, ifelse(stat_out < 1, round(stat_out, digits = round_digit), stat_out))
stat_out <-
  ifelse(is.na(stat_out),NA_character_,
         ifelse(stringr::str_detect(as.character(stat_out), pattern = "\\."),
         stat_out,
         paste0(stat_out, ".00")))
stat_out <-
  ifelse(nchar(strsplit(as.character(stat_out), ".", fixed = TRUE)[[1]][2]) == 1,
         paste0(stat_out, "0"),
         ifelse(nchar(
           strsplit(as.character(stat_out), ".", fixed = TRUE)[[1]][2]
         ) < 1, paste0(stat_out, "00"), ifelse(is.na(stat_out),NA_character_, paste0(stat_out))))
stat_out <-
  ifelse(isTRUE(negative_stat),paste0("-",stat_out),stat_out)
stat_out
}
