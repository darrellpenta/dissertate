#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#'
#' @param .data a dataframe or a named list of data frames
#' @param ... further arguments passed to or from other methods
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var an optional grouping factor
#' @param .btw_var an optional vector naming the between-subjects variable
#' @return a data.frame with a  class attribute for passing to additional
#' @family factorial-design functions
#' @include aov_fgrid_helpers.R
#' @include select_dots-function.R
#' @rdname aov_fgrid
#' @export
#'
aov_fgrid <- function(.data, ...) {
  UseMethod("aov_fgrid", .data)
}

#' Create ANOVA formulas from a filter.grid data frame
#' @rdname aov_fgrid
#' @export

aov_fgrid_generate <-
  function(.data, ..., .dep_var, .grp_var, .btw_var = FALSE) {
    options(stringsAsFactors = FALSE)


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
        aov_groupby_col(
          .data = .data,
          .grp_var = .grp_var
        )
      .data <-
        aov_index_col(.data, .dep_var = .dep_var, .grp_var = .grp_var)

      .data <-
        aov_clean_cols(.data)
      .data
    } else  {
      .data$aov_fixed_form <-
        aov_vars_col(.data)
      .data$aov_error_denom <-
        .data$aov_fixed_form
      # .data <-
      #   aov_terms_col(.data)
      .data <-
        aov_formulate(.data, .dep_var = .dep_var, .grp_var = .grp_var)
      .data <-
        aov_select_col(.data = .data,
                       .dep_var = .dep_var,
                       .grp_var = .grp_var)
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
  }


#' @rdname aov_fgrid
#' @export
aov_fgrid.default <-
  function(.data, .dep_var, .grp_var, .btw_var = FALSE, ...) {
    d_dots <-
      pryr::named_dots(...)
    lapply(d_dots, eval, parent.frame())

    if (is.list(.dep_var) | length(.dep_var) > 1) {
      .data <-
        lapply(.dep_var, function(dv,
                                  data = .data,
                                  grp_var = .grp_var,
                                  btw_var = .btw_var,
                                  ...) {
          dots_ <-
            pryr::named_dots(...)
          lapply(dots_, eval, parent.frame())

          data <-
            aov_fgrid_generate(
              .data = data,
              .dep_var = dv,
              .grp_var = grp_var,
              .btw_var = btw_var
            )
        }) %>%
        dplyr::bind_rows()

      .data
    } else {
      .data <- aov_fgrid_generate(
        .data = .data,
        .dep_var = .dep_var,
        .grp_var = .grp_var,
        .btw_var = .btw_var,
        ... = ...
      )
      .data
    }


  }

#' @rdname aov_fgrid
#' @export
aov_fgrid_vectorized <-
  Vectorize(
    FUN = function(.data, ..., .dep_var, .grp_var, .btw_var = FALSE)
      aov_fgrid.default(.data, ..., .dep_var, .grp_var, .btw_var),
    vectorize.args = c(".dep_var", ".grp_var"),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )


aov_fgrid <-
  function(.data,
           ...,
           .dep_var,
           .grp_var,
           .btw_var = FALSE) {
    dots_afg <-
      pryr::named_dots(...)
    lapply(dots_afg, eval, parent.frame())

    if (is.character(.btw_var) & !(isTRUE(.btw_var))) {
      .data <-
        aov_fgrid_vectorized(
          .data = .data,
          ... = ...,
          .dep_var = .dep_var,
          .grp_var = .grp_var,
          .btw_var = .btw_var
        )
      .data
    } else{
      .data <-
        aov_fgrid_vectorized(
          .data = .data,
          ... = ...,
          .dep_var = .dep_var,
          .grp_var = .grp_var
        )
      .data
    }
    .data <-
      .data %>%
      plyr::ldply(dplyr::bind_rows, .id = "dep_var") %>%
      dplyr::mutate_if(is.factor, "as.character")

    main_number <-
      as.character(as.numeric(factor(.data$dep_var, levels = unique(.data$dep_var))))
    .data$main_number <- as.numeric(main_number)
    .data$set_number <- as.numeric(.data$set_number)
    .data
  }

