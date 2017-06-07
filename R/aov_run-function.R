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


aov_run_ <-
  function(.data, .spec, ...){

    ar_dots <-
      pryr::named_dots(...)
    lapply(ar_dots, eval, parent.frame())

    select_cols<-  c("error","correct","errorND","correctND","unin","uninND","miscel","noresp",unlist(strsplit(.spec$datsel_form,",")))
    edata <-
      .data[,select_cols]

    edata <-
      edata %>%
      dplyr::filter_(.dots = sweet_dots(.spec$filter_form))
    # Columns you want to group by
    grp_cols <- names(edata)[9:ncol(edata)]

    # Convert character vector to list of symbols
    dots <- lapply(grp_cols, as.symbol)

    # Perform frequency counts
   edata <-
    edata %>%
      dplyr::group_by_(.dots=dots) %>%
      dplyr::summarise_at(c("error","correct","errorND","correctND","unin","uninND","miscel","noresp"), "sum")
    edata$errcorr <- rowSums(edata[,c("error","correct")])
    edata$errcorrND <- rowSums(edata[,c("errorND","correctND")])
    edata$pct <- ifelse(edata$error == 0 & edata$errcorr == 0, 0, (edata$error/edata$errcorr)*100)
    edata$pctND <- ifelse(edata$errorND == 0 & edata$errcorrND == 0, 0, (edata$errorND/edata$errcorrND)*100)
    edata$asn  <- (2 * asin(sqrt(edata$pct/100)))
    edata$asnND  <- (2 * asin(sqrt(edata$pctND/100)))
    edata <-
      edata %>%
      magrittr::set_names(value = c(dots,"error","correct","errorND","correctND","unin","uninND","miscel","noresp","errcorr","errcorrND","pct","pctND","asn","asnND"))

    edata <-
      dplyr::ungroup(edata)


    select_cols<-  c("pct","pctND","asn","asnND","unin","uninND","miscel","noresp",unlist(strsplit(.spec$groupby_form,",")))

    edata <-
      edata[,select_cols]

    # Columns you want to group by
    grp_cols <- names(edata)[9:ncol(edata)]

    # Convert character vector to list of symbols
    dots <- lapply(grp_cols, as.symbol)

    # Perform frequency counts
    edata <-
      edata %>%
      dplyr::group_by_(.dots=dots) %>%
      dplyr::summarise_at(c("pct","pctND","asn","asnND", "unin","uninND","miscel","noresp"), "mean") %>%
      dplyr::ungroup()

     select_cols<-
       c(.spec$dep_var,grp_cols)

    edata<-
      edata[,select_cols]


     factr<-
    apply(edata, 2, function(x)isTRUE(length(unique(x)) >= 2))[2:ncol(edata)]
 factr<-c(FALSE,factr)
     edata[,factr] <-
       lapply(edata[,factr], as.factor)

     aov_form <-
       paste('~stats::aov(',.spec$aov_form,',data=edata)')
     aov_form <-
       stats::as.formula(aov_form)
     aov_out <-
       lazyeval::f_eval(aov_form,data=edata)
    aov_out <-
      sweet_tidy(aov_out, interval="sig")
    aov_out
    }


