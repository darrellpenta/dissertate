#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @param .data a data frame or list of named data frames containing source data
#' @param .spec a summary grid dataframe
#' @param ... passed to/from other functions (ignored)
#' @return a data.frame with summary stats
#' @include select_dots-function.R
#' @include flist_to_dots-function.R
#' @include factor_summary-function.R
#' @include factor_summary_helpers-function.R
#' @rdname summary_run
#' @export


summary_run <-
  function(.data, .spec,  ...){
    sr_dots <-
      pryr::named_dots(...)
    lapply(sr_dots, eval, parent.frame())
if(is.list(.data) & !"data.frame" %in% attr(.data,"class")){
  .data <-
    .data[.spec$group_id[1]] %>%
    dplyr::bind_rows()
    .data
}

    # dat_filter <- sweet
    #   list(paste0("~", .spec$filter_form)) %>%
    #   lazyeval::as_f_list(.) %>%
    #   lapply(stats::as.formula)
     .data <-
       .data %>%
       dplyr::select_(.dots = flist_to_dots(.spec$select_form)) %>%
      dplyr::mutate_all("as.character") %>%
      dplyr::mutate_at(paste0(.spec$dep_var),"as.numeric") %>%
      dplyr::filter_(.dots = sweet_dots(.spec$filter_form))

    summary_out <-
    paste0("~factor_summary(.data = .data, .dep_var = .spec$dep_var, .ind_var = paste0(names(.data)[!names(.data) %in% c(.spec$dep_var, .spec$group_id)]), .grp_var = .spec$group_id)")
  summary_out <-
    stats::as.formula(summary_out)
  summary <-
    lazyeval::f_eval(summary_out, data=.data)
   summary<-
     sweet_stat(summary)

    }


