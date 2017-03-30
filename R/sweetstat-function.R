sweetstat. <- function(stat) {
  options(scipen = 999)
  stat <- ifelse(!(is.numeric(stat)), as.numeric(stat), stat)
  
  
  
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



sweetstat <-
  Vectorize(
    FUN = function(stat)
      sweetstat.(stat),
    vectorize.args = c("stat"),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
