standard_error <- function(x){

    x <-
      stats::sd(x) / sqrt(length(x))
  
}
