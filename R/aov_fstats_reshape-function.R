#' Creates a long version of ftable
#'
#' @importFrom magrittr %>%
#' @param .data data to be analysed
#' @param .gather gather ouput
#' @param .col_ord a vector giving the order of dep_var columns
#' @param ... further arguments passed to or from other methods(ignored)
#' @return a data.frame with the results of an fstatstable
#'
#' @rdname aov_fstats_reshape
#' @export

aov_fstats_to_long <- function(.data, ...) {
  .data <-
    .data %>%
    dplyr::ungroup() %>%
    tidyr::separate(
      col = "set_id",
      into = c("grp", "dep_var"),
      sep = "_",
      remove = TRUE
    )

  .data <-
    .data[,sapply(names(.data), FUN=function(x){!x %in% c("stars")})]


  unite_form <-
    paste("~tidyr::unite(.data,  col = fbind,  main_number,  set_number,  dep_var,  label,  term,  sep = '_',  remove = TRUE) %>% dplyr::select(-id, -grp)")
  unite_form <-
    stats::as.formula(unite_form)

  .data <-
    lazyeval::f_eval(unite_form, data = .data)



  data_index <- .data[,c("fbind")] %>%
    unique() %>%
    tibble::rownames_to_column("index_id")

  gsum_form <-
    paste0("~dplyr::group_by(.data, fbind) %>%
           tidyr::spread(., key = group_id, value = stats) %>%
           tidyr::unite(fstat, subject, item, sep = ';', remove = TRUE) %>%
           dplyr::mutate(fstat = paste0('(', fstat, ')'))")
  gsum_form <-
    stats::as.formula(gsum_form)
  .data <-
    lazyeval::f_eval(gsum_form, data = .data) %>%
    dplyr::left_join(data_index, by = "fbind")

  .data$index_id <- as.numeric(.data$index_id)

  sep_form <-
    paste0("~dplyr::arrange(.data, index_id) %>% tidyr::separate(col=fbind,c('main_number','set_number','dep_var','label','term'), sep='_', remove=TRUE)")

  sep_form <-
    stats::as.formula(sep_form)

  .data <-
    lazyeval::f_eval(sep_form, data = .data)

  .data$main_number <- as.numeric(.data$main_number)
  .data$set_number <- as.numeric(.data$set_number)

  arr_form <- paste0("~dplyr::arrange(.data, main_number, set_number, index_id)")
  arr_form <-
    stats::as.formula(arr_form)
  .data<-
    lazyeval::f_eval(arr_form, data = .data)
  .data

}

#' @rdname aov_fstats_reshape
#' @export
aov_fstats_to_wide <-
  function(.data,
           .gather=TRUE,
           .col_ord = c("PCT", "PCTND", "ASN", "ASNND","UNIN","UNINND","MISCEL"),
           ...) {


  .data <-
    .data[,sapply(names(.data), FUN=function(x){!x %in% c("main_number")})]

  l_unite <-
    paste0('~tidyr::unite(.data, col = fbind, set_number, label, term, sep="_", remove=TRUE)')
  l_unite <-
    stats::as.formula(l_unite)
  .data <-
    lazyeval::f_eval(l_unite,data = .data)


  l_df_index <- .data["fbind"] %>%
    unique() %>%
    tibble::rownames_to_column("index_id")
  .data <-
    .data[,sapply(names(.data), FUN=function(x){!x %in% c("index_id")})]

  .data <-
    dplyr::group_by_(.data, .dots = sweet_dots("fbind"))

  .data <-
    tidyr::spread(.data, key = "dep_var", value = "fstat")

  if(isTRUE(.gather)){
  col_num<-ncol(.data)
  col_num <- as.numeric(col_num)
  ldf_out <-
  tidyr::gather_(data = .data,
                 key_col = "dep_var",
                 value_col = "stat",
                 gather_cols = names(.data)[2:col_num]) %>%
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
}else{
  ldf_out <-
  .data %>%
  dplyr::left_join(y = l_df_index, by = "fbind")

  ldf_out$index_id <- as.numeric(ldf_out$index_id)

  ldfout_sep_form <-
    paste("~tidyr::separate(ldf_out,col = fbind, into = c('set_number', 'label','term'),sep = '_',remove = TRUE)")

  ldfout_sep_form <-
    stats::as.formula(ldfout_sep_form)

  ldf_out <-
    lazyeval::f_eval(ldfout_sep_form, data=ldf_out)


  col_ord <- c("index_id","set_number","label","term",.col_ord)

  ldf_out <-ldf_out[,col_ord]

  ldf_out <-
    sort(ldf_out, by = c("set_number","index_id"))

  ldf_out

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
