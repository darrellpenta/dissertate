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


aov_run <-
  function(.data, .spec, ...){
    ar_dots <-
      pryr::named_dots(...)
    lapply(ar_dots, eval, parent.frame())



    grp_by <-
       c(unlist(strsplit(.spec$groupby_form,",")))

          list(paste0("~", .spec$filter_form)) %>%
      lazyeval::as_f_list(.) %>%
      lapply(stats::as.formula)
    edata <-
      .data %>%
      dplyr::group_by_(.dots=c(unlist(strsplit(.spec$groupby_form,",")))) %>%
      dplyr::summarise_at(c("error","correct","errorND","correctND","unin","uninND","miscel","noresp"), "sum")
    edata$errcorr <- rowSums(edata[,c("error","correct")])
    edata$errcorrND <- rowSums(edata[,c("errorND","correctND")])
    edata$pct <- ifelse(edata$error == 0 & edata$errcorr == 0, 0, (edata$error/edata$errcorr)*100)
    edata$pctND <- ifelse(edata$errorND == 0 & edata$errcorrND == 0, 0, (edata$errorND/edata$errcorrND)*100)
    edata$asn  <- (2 * asin(sqrt(edata$pct/100)))
    edata$asnND  <- (2 * asin(sqrt(edata$pctND/100)))
    # data$cnt <- data$error
    # data$cntND <- data$errorND
    edata<-
      edata %>%
      dplyr::group_by_(.dots=c(unlist(strsplit(.spec$groupby_form,",")))) %>%
      dplyr::summarise_at(c("pct","pctND","asn","asnND", "unin","uninND","miscel","noresp"), "mean")


     edata <-
       edata %>%
       dplyr::select_(.dots = flist_to_dots(.spec$select_form)) %>%
      dplyr::mutate_all("as.character") %>%
      dplyr::mutate_at(paste0(.spec$dep_var),"as.numeric") %>%
      dplyr::mutate_if(is.character,"as.factor") %>%
      dplyr::filter_(.dots = sweet_dots(.spec$filter_form))

     aov_out <-
       stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(stats::as.formula(.spec$aov_form))), data = edata)
    aov_out <-
      sweet_tidy(aov_out)
    aov_out
    }


