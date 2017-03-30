aov_combine_fstats_long <- function(ftable) {
  f_table <-
    ftable %>%
    dplyr::ungroup() %>%
    tidyr::separate(
      col = set_id,
      into = c("grp", "dep_var"),
      sep = "_",
      remove = TRUE
    ) %>%
    tidyr::unite(
      col = fbind,
      main_number,
      set_number,
      dep_var,
      label,
      term,
      sep = "_",
      remove = TRUE
    ) %>%
    dplyr::select(-id, -grp) 
  
  f_table_index <-
    f_table %>% 
    dplyr::select(fbind) %>% 
    unique() %>% 
    tibble::rownames_to_column("index_id")
  f_table <-
    f_table %>%
    dplyr::group_by(fbind) %>%
    tidyr::spread(., key = group_factor, value = stats) %>%
    tidyr::unite(fstat, subject, item, sep = ";", remove = TRUE) %>%
    dplyr::mutate(fstat = paste0("(", fstat, ")")) %>% 
    dplyr::left_join(f_table_index, by = "fbind") %>% 
    dplyr::mutate_at("index_id", "as.numeric") %>% 
    dplyr::arrange(index_id) %>%
    tidyr::separate(
      .,
      col = fbind,
      into = c("main_number", "set_number", "dep_var", "label", "term"),
      sep = "_",
      remove = TRUE
    ) %>%
    dplyr::mutate_at(c("main_number", "set_number"), "as.numeric") %>%
    dplyr::arrange(main_number, set_number, index_id)
  f_table
}
