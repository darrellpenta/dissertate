#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom graphics stars
#' @importFrom stats df
#' @param .data data to be analysed
#' @param .spec specs for anovas
#' @param ... further arguments passed to or from other methods
#' @return a data.frame with the results of an anova
#' @include select_dots-function.R
#' @include sweet_tidy-function.R
#' @rdname aov_run
#' @export


aov_run <-
  function(.data, .spec, ...){
    ar_dots <-
      pryr::named_dots(...)
    lapply(ar_dots, eval, parent.frame())
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
      dplyr::mutate_if(is.character,"as.factor") %>%
      dplyr::filter_(.dots = sweet_dots(.spec$filter_form))
    aov_out <-
    stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(stats::as.formula(.spec$aov_form))), data = .data)
    aov_out <-
      sweet_tidy(aov_out)
    aov_out
    }


