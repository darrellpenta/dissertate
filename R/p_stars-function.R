p_stars <-
  function(p, show_ns = TRUE) {
    stars = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
    codes = c("***" , "**", "*", ".", ifelse(isTRUE(show_ns), "n.s.", ""))
    codes[stars]
  }
