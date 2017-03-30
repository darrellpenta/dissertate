aov_fstats_combine <- function(aovdf, stat_ls) {
  
  aovdf_ <-
    aovdf %>% 
    dplyr::ungroup() %>% 
    tibble::as_tibble() %>% 
  tidyr::unite(bind, main_number, set_number, set_id,label, group_factor,id, sep = "_", remove=TRUE) 
  
  aov_out <-
    dplyr::left_join(
      aovdf_ %>% 
        dplyr::filter(term != "Residuals") %>% 
        dplyr::select(bind,term,df, f,p, flabel),
      aovdf_ %>% 
        dplyr::filter(term == "Residuals") %>% 
        dplyr::select(bind,r_df = df, mse),
      by = "bind") %>% 
    dplyr::mutate(
      stats = paste0(
        flabel,"(",df,",",r_df,")", 
        stat_ls["eq"][[1]],f,", ",
        stat_ls["mse"][[1]],mse,", ", 
        stat_ls["p"][[1]],
        ifelse(p == ".001",stat_ls["less"][[1]],stat_ls["eq"][[1]]),p)) %>% 
    dplyr::select(term,stats) 
  aov_out
}
