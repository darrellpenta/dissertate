#' Helper for AOV BW formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#'
#' @param .data a dataframe or a named list of data frames
#' @param ... further arguments passed to or from other methods
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var an optional grouping factor
#' @param .btw_var an bwteen group factor
#' @return a data.frame with a  class attribute for passing to additional
#' @family factorial-design functions
#' @include aov_fgrid_helpers.R
#' @include select_dots-function.R
#' @export
#'

aov_formulate_bw <- function(.data,
                          .dep_var,
                          .grp_var,
                          ...) {
  .data$dlpnum<-row.names(.data)
  .data<-
    .data %>% 
    ddply(.variables = "dlpnum",.fun = function(data, gv=.grp_var){
      
      data$error_term <-
      if(identical(data$aov_fixed_form, data$iv_between)){
        paste0("Error(", gv, ") + ", data$iv_between)
        } else if(data$iv_between == ""){
          paste0("Error(",
                 paste(gv,
                       paste0("(",
                              data$aov_error_denom, ")"),
                       sep = "/"),
                 ")")
      } else { 
          paste0("Error(",paste(gv,paste0("(",data$aov_error_denom,")"),sep = "/"),") + ",data$iv_between)
      }
      data
    })
 

  .data$aov_form <- paste(paste(.dep_var,
                               .data$aov_fixed_form,
                               sep = " ~ "),
                         .data$error_term,
                         sep = " + ")
  .data$dlpnum = NULL
  .data$iv_between = NULL
  .data
 }



#' @rdname aov_form_helpers
#' @export
aov_terms_col_bw <- function(.data, ...) {
    
  
  terms_cols <-
    .data  %>%
    dplyr::bind_rows() %>%
    dplyr::select_(.dots =
                     sweet_dots(sapply(
                       X = c("iv_between",
                             "aov_fixed_form",
                             "aov_error_denom"),
                       USE.NAMES = FALSE,
                       paste,
                       collapse = ","
                     )))
} 


#' Between-subjects variable column
#'
#' @rdname aov_form_helpers
#' @export

iv_between_col <-
  function(.data, .btw_var="cntbal", ...) {

    btw_var <- paste0(.btw_var)
    
    data_btwn <-
      .data %>%
      dplyr::select(dplyr::contains(match = btw_var)) %>%
      dplyr::select(btwn = dplyr::contains(match = "_len"))
    
    data_btwn$btwn <-
      ifelse(data_btwn$btwn == min(data_btwn$btwn), "", paste0(btw_var))
    
    data_btwn$btwn
  }

#' Anova variables column
#'
#' @rdname aov_form_helpers
#' @export

aov_vars_col_bw <- function(.data,...) {
  
  d <-
    .data[grepl(paste0("_aov_term_sel.temp"), names(.data), fixed = TRUE)]
  
d <-
  adply(d,1,
          function(x) {
            stringr::str_c(x[!is.na(x)], collapse = " * ")})

  vars_col<-d$V1
  vars_col
}
#' Anova variables column
#'
#' @rdname aov_form_helpers
#' @export

aov_errdenom_col_bw <- function(.data,.btw_var="cntbal",...) {

str_c_keep <-function(x){
  x<-unlist(x,recursive=TRUE)
  x <- x[!is.na(x)]
  x <-  paste(x,collapse=" * ")
  x}

  
    d <-
      .data[grepl(paste0("_aov_term_sel.temp"), names(.data), fixed = TRUE)]
    d<-
      d[!grepl(paste0(.btw_var), names(d), fixed = TRUE)]

    d$ed<-apply(d,1,str_c_keep)
    d$ed
}

#' Anova variables column
#'
#' @rdname aov_fgrid_between
#' @export

aov_fgrid_between <- function(.data, ...) {
  UseMethod("aov_fgrid_between", .data)
}

aov_fgrid_bw_generate <-
  function(.data, ..., .dep_var, .grp_var, .btw_var = "cntbal") {
    options(stringsAsFactors = FALSE)
    
    
    afg_dots <-
      pryr::named_dots(...)
    lapply(afg_dots, eval, parent.frame())
    
     .data$iv_between <-
        iv_between_col(.data)
      .data$aov_fixed_form <-
        aov_vars_col_bw(.data)
      .data$aov_error_denom <-
        aov_errdenom_col_bw(.data)
      # .data <-
      #   aov_terms_col_bw(.data)
      .data <-
        aov_formulate_bw(
          .data,
          .dep_var = .dep_var,
          .grp_var = .grp_var
        )
      .data <-
        aov_select_col(
          .data = .data,
          .dep_var = .dep_var,
          .grp_var = .grp_var,
          .btw_var = .btw_var
        )
      .data <-
        aov_groupby_col(
          .data = .data,
          .grp_var = .grp_var
        )
      .data <-
        aov_index_col(.data, .dep_var = .dep_var, .grp_var = .grp_var)

      .data <-
        aov_clean_cols(.data)
       .data
  }


#' @rdname aov_fgrid_between
#' @export
#' 
aov_fgrid_between.default <-
  function(.data, .dep_var, .grp_var, .btw_var="cntbal",...) {
    d_dots <-
      pryr::named_dots(...)
    lapply(d_dots, eval, parent.frame())
    
    if (is.list(.dep_var) | length(.dep_var) > 1) {
      .data <-
        lapply(.dep_var, function(dv,
                                  d = .data,
                                  gv = .grp_var,
                                  bv=.btw_var) {
          d <-
            aov_fgrid_bw_generate(
              .data = d,
              .dep_var = dv,
              .grp_var = gv,
              .btw_var=bv
            )
        }) %>%
        dplyr::bind_rows()
      
      .data
    } else {
      .data <- aov_fgrid_bw_generate(
        .data = .data,
        .dep_var = .dep_var,
        .grp_var = .grp_var,
        .btw_var=.btw_var
      )
      .data
    }
    
  }
#' @rdname aov_fgrid_between
#' @export
aov_fgrid_between <-
  function(.data,
           .dep_var,
           .grp_var,
           .btw_var="cntbal",
           ...) {
    dots<-
      pryr::named_dots(...)
    lapply(dots, eval, parent.frame())
    dots<-list(.data,.btw_var,unlist(dots))
    .data <-
      mapply(aov_fgrid_between.default,
             .dep_var = .dep_var,
             .grp_var = .grp_var,
             MoreArgs = dots,
             SIMPLIFY = FALSE,
             USE.NAMES = TRUE) %>%
      plyr::ldply(dplyr::bind_rows, .id = "dep_var") %>%
      dplyr::mutate_if(is.factor, "as.character")
    
    main_number <-
      as.character(as.numeric(factor(.data$dep_var,
                                     levels = unique(.data$dep_var))))
    .data$main_number <- as.numeric(main_number)
    .data$set_number <- as.numeric(.data$set_number)
    .data
  }

