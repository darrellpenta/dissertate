aov_fstats_long_to_wide <- function(.data,...) {
 # gathered = TRUE dropped for now

   dvs <- .data["dep_var"] %>%
     unique()


  l_df <-
    .data %>%
    dplyr::ungroup()
  l_df <-
    l_df[,sapply(names(l_df), FUN=function(x){!x %in% c("main_number")})]

  l_unite <-
    paste0("~tidyr::unite(l_df, col = fbind, set_number, label, term, sep = '_', remove = TRUE)")
  l_unite <-
    stats::as.formula(l_unite)
  l_df <-
    lazyeval::f_eval(l_unite,data = l_df)


  l_df_index <- l_df["fbind"] %>%
    unique() %>%
    tibble::rownames_to_column("index_id")
  l_df <-
    l_df[,sapply(names(l_df), FUN=function(x){!x %in% c("index_id")})]

   l_df <-
     group_by_(l_df, .dots = sweet_dots("fbind"))

   l_df <-
    tidyr::spread(l_df, key = "dep_var", value = "fstat")

   if(isTRUE(gather)){
   colnums<-
      ncol(l_df)
   gather_form <- paste("~tidyr::gather(l_df, dep_var, stat, 2:colnums)")
   gather_form <-
     stats::as.formula(gather_form)
    ldf_out <-
      lazyeval::f_eval(gather_form, data = l_df) %>%
      dplyr::left_join(y = l_df_index, by = "fbind")

    ldf_out$index_id <- as.numeric(ldf_out$index_id)

    ldfout_sep_form <-
      paste("~tidyr::separate(ldf_out,col = fbind, into = c('set_number', 'label','term'),sep = '_',remove = TRUE)")

    ldfout_sep_form <-
      stats::as.formula(ldfout_sep_form)

     ldf_out <-
       lazyeval::f_eval(ldfout_sep_form, data=ldf_out)

     ldf_out <-
       ldf_out[,sapply(names(ldf_out), FUN=function(x){x %in% c("index_id","set_number","label","term","dep_var","stat")})]


     ldf_out <-
       sort(ldf_out, by = c("set_number","index_id","dep_var"))

    ldf_out
    } else{
  #   ldf_out <-
  #     l_df %>%
  #     dplyr::left_join(y = l_df_index, by = "fbind") %>%
  #     dplyr::mutate_at("index_id", "as.numeric") %>%
  #     tidyr::separate(
  #       .,
  #       col = fbind,
  #       into = c("set_number", "label", "term"),
  #       sep = "_",
  #       remove = TRUE
  #     ) %>%
  #     dplyr::select(index_id, set_number, label, term, dplyr::everything()) %>%
  #     dplyr::arrange(set_number, index_id)
  #   ldf_out
   }
}
