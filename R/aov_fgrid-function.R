#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#'
#' @param .data a dataframe or a named list of data frames
#' @param ... further arguments passed to or from other methods
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var an optional grouping factor
#' @return a data.frame with a  class attribute for passing to additional
#' @family factorial-design functions
#' @include aov_fgrid_helpers.R
#' @include select_dots-function.R
#' @rdname aov_fgrid
#' @export
#'
#'
aov_fgrid <- function(.data, ...) {
  UseMethod("aov_fgrid", .data)
}

aov_fgrid_generate <-
  function(.data, ..., .dep_var, .grp_var) {
    options(stringsAsFactors = FALSE)
    
    
    afg_dots <-
      pryr::named_dots(...)
    lapply(afg_dots, eval, parent.frame())
  
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



#' @rdname aov_fgrid
#' @export
#' 
aov_fgrid.default <-
  function(.data, .dep_var, .grp_var, ...) {
    d_dots <-
      pryr::named_dots(...)
    lapply(d_dots, eval, parent.frame())
    
    if (is.list(.dep_var) | length(.dep_var) > 1) {
      .data <-
        lapply(.dep_var, function(dv,
                                  d = .data,
                                  gv = .grp_var,
                                  ...) {
          dots_ <-
            pryr::named_dots(...)
          lapply(dots_, eval, parent.frame())
          
          d <-
            aov_fgrid_generate(
              .data = d,
              .dep_var = dv,
              .grp_var = gv,
              ... = ...
            )
        }) %>%
        dplyr::bind_rows()
      
      .data
    } else {
      .data <- aov_fgrid_generate(
        .data = .data,
        .dep_var = .dep_var,
        .grp_var = .grp_var,
        ... = ...
      )
      .data
    }
    
  }

#' @rdname aov_fgrid
#' @export
aov_fgrid <-
  function(.data,
           .dep_var,
           .grp_var,
           ...) {
    dots<-
      pryr::named_dots(...)
    lapply(dots, eval, parent.frame())
    dots<-list(.data,unlist(dots))
    .data <-
        mapply(aov_fgrid.default,
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

