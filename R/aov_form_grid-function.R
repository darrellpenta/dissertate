#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#'
#' @param .data a  .dataframe
#' @param ... further arguments passed to or from other methods
#' @param .dep_var a vector naming the depend. var.
#' @param .btw_var an optional vector naming the between-subjects variable
#' @param .grp_var an optional grouping factor
#' @return a data.frame with a  class attribute for passing to additional
#' @family factorial-design functions
#' @include aov_form_helpers-function.R
#' @include select_dots-function.R
#' @rdname aov_form_grid
#' @export
#'
aov_form_grid <- function(.data, ...) {
  UseMethod("aov_form_grid", .data)
}


#' Create ANOVA formulas from a filter.grid data frame
#'

#' @rdname aov_form_grid
#' @export

aov_form_grid.filter.grid <-
  function(.data, ..., .dep_var, .grp_var, .btw_var = FALSE) {
    options(stringsAsFactors = FALSE)

    assertthat::validate_that(
      is.data.frame(.data) |
        tibble::is.tibble(.data),
      assertthat::has_attr(.data, "names")
    )

    afg_dots <-
      pryr::named_dots(...)
    lapply(afg_dots, eval, parent.frame())

    # Prepare between-IV columns for ANOVA formula ----------------------------
    if (is.character(.btw_var) & !(isTRUE(.btw_var))) {

      .data$iv_between <-
        iv_between_col(.data, .btw_var)
      .data$aov_fixed_form <-
        aov_vars_col(.data, .rm_col = FALSE)
      .data$aov_error_denom <-
        aov_vars_col(.data, .btw_var)

      .data <-
        aov_terms_col(.data, .btw_var = .btw_var)
      .data<-
        aov_formulate(.data, .dep_var = .dep_var, .grp_var = .grp_var, .btw_var = .btw_var)
      .data
    } else  {
      .data$aov_fixed_form <-
        aov_vars_col(.data, .rm_col = FALSE)
      .data$aov_error_denom <-
        aov_vars_col(.data, .rm_col = FALSE)
      .data <-
        aov_terms_col(.data)
      .data<-
        aov_formulate(.data, .dep_var = .dep_var, .grp_var = .grp_var)
    }
  }
#'     .data$select_formula <-
#'       aov_select_col(
#'         select_data = .data,
#'         s_dep_var = .dep_var,
#'         s_grp_factor = grp_factor
#'       )


#'     # Create index xolumn -----------------------------------------------------
#'     data <-  tibble::rownames_to_column(df = .data,
#'                                         var = "set_number")
#'     data <-
#'       data %>%
#'       dplyr::mutate(
#'         group_factor = paste0(grp_factor),
#'         aid1 = toupper(group_factor),
#'         aid2 = toupper(paste0(.dep_var))
#'       ) %>%
#'       tidyr::unite(set_id,
#'                    aid1,
#'                    aid2,
#'                    sep = "_",
#'                    remove = TRUE)
#'
#'
#'     data <- #drops len (unneeded)
#'       data %>%
#'       dplyr::select(-dplyr::contains(match = "_len")) %>%
#'       dplyr::select(-dplyr::contains(match = "_nm")) %>%
#'       dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
#'       dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
#'       dplyr::select(-dplyr::contains(match = "_denom")) %>%
#'       dplyr::select(-dplyr::contains(match = "index")) %>%
#'       dplyr::select(-dplyr::contains(match = "sums")) %>%
#'       dplyr::select(set_number, label, dplyr::everything()) %>%
#'       dplyr::mutate(.dep_var = paste0(.dep_var))
#'
#'     data <-
#'       if (isTRUE(use_btw_var)) {
#'         data %>%
#'           dplyr::select(-dplyr::contains(match = "_between"))
#'       } else{
#'         data
#'       }
#'     data
#'   }
#'
#' #' Alternate version of \code{aov_forms}
#'
#' #' @family factorial-design functions
#' #' @rdname aov_forms
#' #' @export
#' #'
#' aov_formply <-
#'    function(.data,
#'             .dep_var,
#'             grp_factor,
#'             ...) {
#'
#'  dots <- list(...)
#'
#'  if(length(dots) == 0){
#'      out <-
#'        aov_form.default(
#'          .data = .data,
#'          .dep_var = .dep_var,
#'          grp_factor = grp_factor
#'        )
#'      out
#'  }else{
#'
#'    ind_var_btwn = paste0(dots)
#'    out<-
#'    aov_forms.default(
#'        .data = .data,
#'        .dep_var = .dep_var,
#'        ind_var_btwn = ind_var_btwn,
#'        grp_factor = grp_factor
#'      )
#'    out
#'  }
#'      out <-
#'        out %>%
#'        plyr::ldply(bind_rows, .id = ".dep_var") %>%
#'        dplyr::mutate_if(is.factor, "as.character")
#'      out$main_number <-
#'        as.character(as.numeric(factor(out$dep_var,
#'                                       levels = unique(out$dep_var))))
#'      out<-
#'        out %>%
#'        dplyr::select(main_number, set_number, set_id, dplyr::everything()) %>%
#'        dplyr::mutate_at(c("main_number", "set_number"), "as.numeric")
#'      out
#'    }
#'
#'



