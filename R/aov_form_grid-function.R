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
      .data <-
        aov_formulate(
          .data,
          .dep_var = .dep_var,
          .grp_var = .grp_var,
          .btw_var = .btw_var
        )
      .data <-
        aov_select_col(
          .data = .data,
          .dep_var = .dep_var,
          .grp_var = .grp_var,
          .btw_var = .btw_var
        )
      .data <-
        aov_index_col(.data, .dep_var = .dep_var, .grp_var = .grp_var)

      .data <-
        aov_clean_cols(.data, .dep_var = .dep_var)
      .data
    } else  {
      .data$aov_fixed_form <-
        aov_vars_col(.data, .rm_col = FALSE)
      .data$aov_error_denom <-
        aov_vars_col(.data, .rm_col = FALSE)
      .data <-
        aov_terms_col(.data)
      .data <-
        aov_formulate(.data, .dep_var = .dep_var, .grp_var = .grp_var)
      .data <-
        aov_select_col(.data = .data,
                       .dep_var = .dep_var,
                       .grp_var = .grp_var)
      .data <-
        aov_index_col(.data, .dep_var = .dep_var, .grp_var = .grp_var)
      .data <-
        aov_clean_cols(.data, .dep_var = .dep_var)
      .data
    }
  }
