sweetp. = function(stat,
                   not_as_range,
                   snip_lead_zero) {
  options(scipen = 999)
  
  if (missing(not_as_range)) {
    not_as_range = TRUE
  }
  
  if (missing(snip_lead_zero)) {
    snip_lead_zero = TRUE
  }
  
  
  stat <-
    ifelse(is.character(stat),
           as.numeric(stat), stat)
  
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




sweetp <-
  Vectorize(
    FUN = function(stat, not_as_range, snip_lead_zero)
      sweetp.(stat, not_as_range, snip_lead_zero),
    vectorize.args = c("stat"),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
