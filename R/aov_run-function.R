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
#' @include flist_to_dots-function.R
#' @include sweet_tidy-function.R
#' @rdname aov_run
#' @export
aggregrate_data <-
  function(.data, .spec, ...){
    ag_dots <-
      pryr::named_dots(...)
    lapply(ag_dots, eval, parent.frame())
        data <-
      .data %>%
      dplyr::group_by_(.dots = flist_to_dots(.spec$groupby_form))
    data <-
      data %>%
      dplyr::summarise_at(c("error","correct","errorND","correctND","unin","uninND","miscel","noresp"), "sum")
    data$errcorr <- rowSums(data[,c("error","correct")])
    data$errcorrND <- rowSums(data[,c("errorND","correctND")])
    data$pct <- ifelse(data$error == 0 & data$errcorr == 0, 0, (data$error/data$errcorr)*100)
    data$pctND <- ifelse(data$errorND == 0 & data$errcorrND == 0, 0, (data$errorND/data$errcorrND)*100)
    data$asn  <- (2 * asin(sqrt(data$pct/100)))
    data$asnND  <- (2 * asin(sqrt(data$pctND/100)))
    # data$cnt <- data$error
    # data$cntND <- data$errorND

    data<-
      data %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(paste0(.spec$group_id),"as.factor") %>%
      dplyr::group_by_(.dots = flist_to_dots(.spec$groupby_form)) %>%
      dplyr::summarise_at(c("pct","pctND","asn","asnND", "unin","uninND","miscel","noresp"), "mean")

    data<-
      data %>%
      dplyr::mutate_at(paste0(.spec$group_id),"as.character")
    data <-
      dplyr::ungroup(data)
    data
  }

#' @rdname aov_run
#' @export


aov_run <-
  function(.data, .spec, ...){
    ar_dots <-
      pryr::named_dots(...)
    lapply(ar_dots, eval, parent.frame())
# if(is.list(.data) & !"data.frame" %in% attr(.data,"class")){
#   .data <-
#     .data[.spec$group_id[1]] %>%
#     dplyr::bind_rows()
#     .data
# }


    # dat_filter <- sweet
    #   list(paste0("~", .spec$filter_form)) %>%
    #   lazyeval::as_f_list(.) %>%
    #   lapply(stats::as.formula)
d <-
  aggregrate_data(.data = .data,
                  .spec= .spec)

     d <-
       d %>%
       dplyr::select_(.dots = flist_to_dots(.spec$select_form)) %>%
      dplyr::mutate_all("as.character") %>%
      dplyr::mutate_at(paste0(.spec$dep_var),"as.numeric") %>%
      dplyr::mutate_if(is.character,"as.factor") %>%
      dplyr::filter_(.dots = sweet_dots(.spec$filter_form))

    aov_out<-
      stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(stats::as.formula(.spec$aov_form))), data=d)
    aov_out <-
      sweet_tidy(aov_out)
    aov_out
    }


