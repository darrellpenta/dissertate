#' Helper functions for aov_forms
#'
#' @param data data passed from aov_forms
#' @param ... data to pass to or from other functions
#' @return an aov-ready column
#' @rdname formula_column
#' @family formula_column functions


#' Between-subjects variable column
#'
#' @param btw_var a between-subjects variable
#' @family formula_column functions
#' @rdname formula_column
#' @export

iv_between_col <-
  function(data, btw_var) {
    data %>%
      dplyr::select(dplyr::contains(match = btw_var)) %>%
      dplyr::select(btwn = dplyr::contains(match = "_len")) %>%
      dplyr::mutate(btwn = ifelse(btwn == min(.$btwn), "", paste0(btw_var))) %>%
      .$btwn
  }

#' Anova variables column
#'
#' @family formula_column functions
#' @rdname formula_column
#' @include select_dots-function.R
#' @export
#'
aov_vars_col <-
  function(data, ...) {
    dots <- list(...)
    if (length(dots) == 0) {
      out <-
        colnames(dplyr::select(data, dplyr::contains(match = "_nm"))) %>%
        stringr::str_replace_all("_nm", "") %>%
        stringr::str_trim(., side = "both")
    } else{
      removecol <- paste0(dots)
      out <-
        dplyr::select(data, -starts_with(removecol)) %>%
        dplyr::select(dplyr::contains(match = "_nm")) %>%
        colnames() %>%
        stringr::str_replace_all("_nm", "") %>%
        stringr::str_trim(., side = "both")
    }

    out <-
      out %>%
      sapply(
        simplify = "vector",
        USE.NAMES = FALSE,
        FUN = function(v,
                       av_dat = data) {
          av_dat_out <-
            av_dat %>%
            dplyr::select_(.dots = select_dots(item1 = paste0(v, "_len")))

          av_dat_out[[1]] <-
            ifelse(av_dat_out[[1]] == 1,
                   NA,
                   paste(v))
        }
      ) %>%
      apply(
        MARGIN = 1,
        FUN = function(x) {
          x <- paste(stats::na.omit(stringr::str_trim(x, side = "both")),
                     collapse = " * ")
        }
      )

  }

#' Anova select column
#'
#' @param s_dep_var dependent variable selector
#' @param s_grp_factor name of the grouping factor
#' @family formula_column functions
#' @rdname formula_column
#' @include select_dots-function.R
#' @export
#'
#'
aov_select_col <-
  function(data, s_dep_var, s_grp_factor) {
    select_data_out <-
      c(
        colnames(dplyr::select(data, dplyr::contains(match = "_nm"))) %>% stringr::str_replace_all("_nm", "") %>%
          stringr::str_trim(., side = "both"),
        s_dep_var,
        s_grp_factor
      )
    select_data_out <-
      paste0("~list(",
             paste("~", select_data_out, collapse = ",",
                   sep = ""),
             ")")

  }
