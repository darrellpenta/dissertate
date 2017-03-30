aov_fstats_long_to_wide <- function(long_fs, gathered = TRUE) {
  deps <-
    long_fs %>%
    dplyr::select(dep_var) %>%
    unique()
  
  
  l_df <-
    long_fs %>%
    dplyr::ungroup() %>%
    dplyr::select(-main_number) %>%
    tidyr::unite(
      col = fbind,
      set_number,
      label,
      term,
      sep = "_",
      remove = TRUE
    )
  
  l_df_index <-
    l_df %>%
    dplyr::select(fbind) %>%
    unique() %>%
    tibble::rownames_to_column("index_id")
  l_df <-
    l_df %>%
    dplyr::select(-index_id) %>%
    dplyr::group_by(fbind) %>%
    tidyr::spread(., key = dep_var, value = fstat) %>%
    dplyr::select_(.dots = select_f(c("fbind", paste(
      unlist(deps), sep = ","
    ))))
  if (isTRUE(gathered)) {
    ldf_out <-
      l_df %>%
      tidyr::gather(dep_var, stat, 2:ncol(.)) %>%
      dplyr::left_join(y = l_df_index, by = "fbind") %>%
      dplyr::mutate_at("index_id", "as.numeric") %>%
      tidyr::separate(
        .,
        col = fbind,
        into = c("set_number", "label", "term"),
        sep = "_",
        remove = TRUE
      ) %>%
      dplyr::select(index_id, set_number, label, term, dep_var, stat) %>%
      dplyr::arrange(set_number, index_id, dep_var)
    ldf_out
  } else{
    ldf_out <-
      l_df %>%
      dplyr::left_join(y = l_df_index, by = "fbind") %>%
      dplyr::mutate_at("index_id", "as.numeric") %>%
      tidyr::separate(
        .,
        col = fbind,
        into = c("set_number", "label", "term"),
        sep = "_",
        remove = TRUE
      ) %>%
      dplyr::select(index_id, set_number, label, term, dplyr::everything()) %>%
      dplyr::arrange(set_number, index_id)
    ldf_out
  }
}
