
#' Create sumamry formulas from a filter.grid
#'
#' Creates a dataframe of formulas for summary from a filter grid
#'
#' @param .data a dataframe or a named list of data frames
#' @param ... further arguments passed to or from other methods
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var an optional grouping factor
#' @return a data.frame with a  class attribute for passing to additional
#' @family factorial-design functions
#' @include summary_form_helpers-function.R
#' @include select_dots-function.R
#' @rdname summary_form_grid
#' @export
#'
summary_form_grid <- function(.data, ...) {
  UseMethod("summary_form_grid", .data)
}

#' @rdname summary_form_grid
#' @export

summary_form_grid_generate <-
  function(.data, ..., .dep_var, .grp_var) {
    options(stringsAsFactors = FALSE)


    afg_dots <-
      pryr::named_dots(...)
    lapply(afg_dots, eval, parent.frame())


      .data$summary_ivs <-
        summary_vars_col(.data)
      .data <-
        summary_select_col(.data = .data,
                       .dep_var = .dep_var,
                       .grp_var = .grp_var)
      .data <-
        summary_index_col(.data, .dep_var = .dep_var, .grp_var = .grp_var)
      .data <-
        summary_clean_cols(.data)
      .data

  }


#' @rdname summary_form_grid
#' @export
summary_form_grid.default <-
  function(.data, .dep_var, .grp_var,  ...) {
    d_dots <-
      pryr::named_dots(...)
    lapply(d_dots, eval, parent.frame())

    if (is.list(.dep_var) | length(.dep_var) > 1) {
      .data <-
        lapply(.dep_var, function(dv,
                                  data_ = .data,
                                  grp_var_ = .grp_var,
                                  ...) {
          dots_ <-
            pryr::named_dots(...)
          lapply(dots_, eval, parent.frame())

          data_ <-
            summary_form_grid_generate(
              .data = data_,
              .dep_var = dv,
              .grp_var = grp_var_
            )
        }) %>%
        dplyr::bind_rows()

      .data
    } else {
      .data <- summary_form_grid_generate(
        .data = .data,
        .dep_var = .dep_var,
        .grp_var = .grp_var,
        ... = ...
      )
      .data
    }


  }

#' @rdname summary_form_grid
#' @export
summary_form_grid_vectorized <-
  Vectorize(
    FUN = function(.data, ..., .dep_var, .grp_var)
      summary_form_grid.default(.data, ..., .dep_var, .grp_var),
    vectorize.args = c(".dep_var", ".grp_var"),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )


summary_form_grid <-
  function(.data,
           ...,
           .dep_var,
           .grp_var) {
    dots_afg <-
      pryr::named_dots(...)
    lapply(dots_afg, eval, parent.frame())

      .data <-
        summary_form_grid_vectorized(
          .data = .data,
          ... = ...,
          .dep_var = .dep_var,
          .grp_var = .grp_var
        )
      .data

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

