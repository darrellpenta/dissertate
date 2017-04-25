#' Creates a long version of ftable
#'
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom graphics stars
#' @importFrom stats df
#' @param .data data to be analysed
#' @param ... further arguments passed to or from other methods(ignored)
#' @return a data.frame with the results of an fstatstable
#'
#' @rdname aov_fstats_to_long
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
