 now <-  function(x){
 ifelse(missing(x), paste0(format(Sys.time(), "%m_%d_%R")),paste0(format(Sys.time(), "%m_%d_%R")) )
 }
