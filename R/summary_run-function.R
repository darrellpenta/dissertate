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
    #
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


    # factr<-
    #   apply(edata, 2, function(x)isTRUE(length(unique(x)) >= 2))[2:ncol(edata)]
    # factr<-c(FALSE,factr)
    # edata[,factr] <-
    #   lapply(edata[,factr], as.factor)
    #
    #  .data <-
    #    .data %>%
    #    dplyr::select_(.dots = flist_to_dots(.spec$select_form)) %>%
    #   dplyr::mutate_all("as.character") %>%
    #   dplyr::mutate_at(paste0(.spec$dep_var),"as.numeric") %>%
    #   dplyr::filter_(.dots = sweet_dots(.spec$filter_form))

    summary_out <-
    paste0("~factor_summary(.data = edata, .dep_var = .spec$dep_var, .ind_var = paste0(names(.data)[!names(.data) %in% c(.spec$dep_var, .spec$group_id)]), .grp_var = .spec$group_id)")
  summary_out <-
    stats::as.formula(summary_out)
  summary <-
    lazyeval::f_eval(summary_out, data=edata)
   # summary<-
   #   sweet_stat(summary)

    }


