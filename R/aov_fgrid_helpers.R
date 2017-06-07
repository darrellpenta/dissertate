#' AOV form helpers
#'
#' @param .data a  .dataframe
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var the grouping factor
#' @param ... further arguments passed to or from other methods
#' @return a data.frame with a  class attribute for passing to additional
#' @include sweet_dots-function.R
#' @rdname aov_form_helpers
#' @export
#'
aov_formulate <- function(.data,
                          .dep_var,
                          .grp_var,
                          ...) {
  af_dots <-
    pryr::named_dots(...)
  lapply(af_dots, eval, parent.frame())

    error_term <-
      paste0("Error(",
             paste(.grp_var,
                   paste0("(", .data$aov_error_denom, ")"),
                   sep = "/"),
             ")")
    
    .data$error_term <- error_term
    
    
    aov_formula <-
      paste(paste(.dep_var,
                  .data$aov_fixed_form,
                  sep = " ~ "),
            .data$error_term,
            sep = " + ")
    .data$aov_form <- aov_formula
    .data
  }


#' @rdname aov_form_helpers
#' @export
aov_terms_col <- function(.data,  ...) {
  terms_cols <-
    .data %>%
    dplyr::bind_rows() %>%
    dplyr::select_(.dots =
                     sweet_dots(sapply(
                       X = c("aov_fixed_form",
                             "aov_error_denom"),
                       USE.NAMES = FALSE,
                       paste,
                       collapse = ","
                     )))
}


#' Anova variables column
#'
#' @rdname aov_form_helpers
#' @export

aov_vars_col <- function(.data, ...) {
  fc_dots <-
    pryr::named_dots(...)
  
  lapply(fc_dots, eval, parent.frame())
  
  d <-
    .data[grepl(paste0("_aov_term_sel.temp"), names(.data), fixed = TRUE)]
  vars_col <-
    apply(d,1,
          function(x) {
            stringr::str_c(x[!is.na(x)], collapse = " * ")})
  vars_col
  
}

#' Anova select column
#'
#' @rdname aov_form_helpers
#' @export
#'
aov_select_col <-
  function(.data, .grp_var, ...) {
    selc_dots <-
      pryr::named_dots(...)
    
    
    selc_dots <-
      pryr::named_dots(...)
    
    lapply(selc_dots, eval, parent.frame())
    
    d <-
      .data[grepl(paste0("_dat_sel.temp"), names(.data), fixed = TRUE)]
    
    d$grp_var <- paste0(.grp_var)
    
    
    d[] <-
      lapply(d[], function(x) x<- ifelse(is.na(x),NA,paste0(x)))
    
    d$datsel <-
      apply(d,1,
            function(x) {
              stringr::str_c(x[!is.na(x)], collapse = ",")})
    
    .data$datsel_form <-
      d$datsel
    
    .data
    
  }

#' Anova aggregrate dv group-by  column
#'
#' @rdname aov_form_helpers
#' @export
#'
aov_groupby_col <-
  function(.data, .grp_var, ...) {
    selc_dots <-
      pryr::named_dots(...)
    
    lapply(selc_dots, eval, parent.frame())
    
    d <-
      .data[grepl(paste0("_term_sel.temp"), names(.data), fixed = TRUE)]
    
    d$grp_var <- paste0(.grp_var)
    
    
    d[] <-
      lapply(d[], function(x) x<- ifelse(is.na(x),NA,paste0(x)))
    
    d$gbform <-
      apply(d,1,
            function(x) {
              stringr::str_c(x[!is.na(x)], collapse = ",")})
    
    .data$groupby_form <-
      d$gbform
    .data
  }

#' @rdname aov_form_helpers
#' @export
aov_index_col <- function(.data, .dep_var, .grp_var, ...) {
  i_dots <-
    pryr::named_dots(...)
  
  lapply(i_dots, eval, parent.frame())
  
  
  .data <-
    tibble::rownames_to_column(.data, "set_number")
  
  set_id <- paste(toupper(.grp_var), toupper(.dep_var), sep = "_")
  group_id <- paste(.grp_var)
  dep_var <- paste(.dep_var)
  .data$set_id <-
    set_id
  .data$group_id <-
    group_id
  .data$dep_var <-
    dep_var
  
  .data
  
}



#' @rdname aov_form_helpers
#' @export
aov_clean_cols <- function(.data, ...) {
  cc_dots <-
    pryr::named_dots(...)
  
  lapply(cc_dots, eval, parent.frame())
  .data <- #drops len (unneeded)
    .data %>%
    dplyr::select(-dplyr::contains(match = "_len")) %>%
    dplyr::select(-dplyr::contains(match = "_nm")) %>%
    dplyr::select(-dplyr::contains(match = ".temp")) %>%
    dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
    dplyr::select(-dplyr::contains(match = "_denom")) %>%
    dplyr::select(-dplyr::contains(match = "aov_fixed_form")) %>%
    dplyr::select(-dplyr::contains(match = "error_term"))
  
  .data
}
