#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom graphics stars
#' @importFrom stats df
#' @param .data data to be analysed
#' @param .spec specs for anovas
#' @param .custom_dv_col names of columns of DVs (not already specified in datsel_form to use), in a list or vector
#' @param .custom_aggr_funs names of funs to use to aggregrate data in a vector
#' @param ... further arguments passed to or from other methods
#' @return a data.frame with the results of an anova
#' @include select_dots-function.R
#' @include flist_to_dots-function.R
#' @include sweet_tidy-function.R
#' @rdname aov_run
#' @export


aov_custom <-
  function(.data, .spec, ..., .custom_dv_col, .custom_aggr_funs){
    
    ar_dots <-
      pryr::named_dots(...)
    lapply(ar_dots, eval, parent.frame())
    
    select_cols<- c(unlist(.custom_dv_col),unlist(strsplit(.spec$datsel_form,",")))

      edata <-
      .data[,select_cols]
    
    edata <-
      edata %>%
      dplyr::filter_(.dots = sweet_dots(.spec$filter_form))
    # Columns you want to group by (defaults to length(select_cols)+1)
    start <- length(.custom_dv_col) +1
    end<-ncol(edata)
    grp_cols <- names(edata)[start:end]
    
    # Convert character vector to list of symbols
    dots <- lapply(grp_cols, as.symbol)
    
    # Perform frequency counts
    edata <-
      edata %>%
      dplyr::group_by_(.dots=dots) %>%
      dplyr::summarise_at(c(unlist(.custom_dv_col)), paste(.custom_aggr_funs[1])
      )
    
     # edata <-
     #  edata %>%
     #  magrittr::set_names(value = c(dots,"error","correct","errorND","correctND","unin","uninND","miscel","noresp","errcorr","errcorrND","pct","pctND","asn","asnND"))
     # 
    edata <-
      dplyr::ungroup(edata)
    
    
    select_cols<-  c(unlist(.custom_dv_col),unlist(strsplit(.spec$groupby_form,",")))
    
    edata <-
      edata[,select_cols]
    
    # Columns you want to group by
    start <- length(.custom_dv_col) +1
    end<-ncol(edata)
    grp_cols <- names(edata)[start:end]
    
    # Convert character vector to list of symbols
    dots <- lapply(grp_cols, as.symbol)
    
    # Perform frequency counts
    edata <-
      edata %>%
      dplyr::group_by_(.dots=dots) %>%
      dplyr::summarise_at(c(unlist(.custom_dv_col)), paste(.custom_aggr_funs)[2]) %>%
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
      sweet_tidy(aov_out)
    aov_out
  }


