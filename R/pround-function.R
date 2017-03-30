pround <- function(p) {
  round(round(p / as.numeric(paste0(
    "0.000", substr(
      stringr::str_replace(p, "([\\.0]+)([1-9]+)", replacement = "\\2"),
      1,
      1
    )
  ))) * as.numeric(paste0(
    "0.000", substr(
      stringr::str_replace(p, "([\\.0]+)([1-9]+)",
                           replacement = "\\2"),
      1,
      1
    )
  )), digits = 3)
}