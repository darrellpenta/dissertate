#' Summary form helpers
#'
#' @param .data a  .dataframe
#' @param ... further arguments passed to or from other methods
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var the grouping factor
#' @return a data.frame with a  class attribute for passing to additional
#' @include sweet_dots-function.R
#' @rdname summary_form_helpers
#' @export

summary_vars_col <- function(.data, ...) {
  fc_dots <-
    pryr::named_dots(...)

  lapply(fc_dots, eval, parent.frame())


    out <-
      colnames(dplyr::select(.data, dplyr::contains(match = "_nm"))) %>%
      stringr::str_replace_all("_nm", "")
    out <-
      stringr::str_trim(out, side = "both")
    out

  out_data <- .data
  out <-
    out %>%
    sapply(
      simplify = "vector",
      USE.NAMES = FALSE,
      FUN = function(.v,
                     .av_dat = out_data) {
        select_v <-
          paste0(.v, "_len")

        av_dat_out <-
          tibble::tibble(item1 =
                           .av_dat[, sapply(names(.av_dat), grepl, select_v)])

        av_dat_out[[1]] <-
          ifelse(av_dat_out[[1]] == 1,
                 NA,
                 paste0('"',.v,'"'))
      }

    )
  out <- apply(
    out,
    MARGIN = 1,
    FUN = function(x) {
      x <- paste(stats::na.omit(stringr::str_trim(x, side = "both")),
                 collapse = ",")
    }
  )
  out
}

#' Anova select column
#'
#' @rdname summary_form_helpers
#' @export
#'
summary_select_col <-
  function(.data, .dep_var, .grp_var, ...) {
    selc_dots <-
      pryr::named_dots(...)

    lapply(selc_dots, eval, parent.frame())

    select_data_out <-
      colnames(dplyr::select(.data,
                             dplyr::contains(match = "_nm"))) %>%
      stringr::str_replace_all("_nm", "")

    select_data_out <-
      stringr::str_trim(select_data_out,
                        side = "both")

    select_data_out <-
      c(select_data_out,
        .dep_var,
        .grp_var)
    select_data_out <-
      paste0("list(",
             paste("~",select_data_out, collapse = ",",
                   sep = ""),
             ")")


    .data$select_form <-
      select_data_out
    .data
  }

#' @rdname summary_form_helpers
#' @export
summary_index_col <- function(.data, .dep_var, .grp_var, ...) {
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



#' @rdname summary_form_helpers
#' @export
summary_clean_cols <- function(.data, ...) {
  cc_dots <-
    pryr::named_dots(...)

  lapply(cc_dots, eval, parent.frame())
  .data <- #drops len (unneeded)
    .data %>%
    dplyr::select(-dplyr::contains(match = "_len")) %>%
    dplyr::select(-dplyr::contains(match = "_nm")) %>%
    dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
    dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
    dplyr::select(-dplyr::contains(match = "_denom")) %>%
    dplyr::select(-dplyr::contains(match = "summary_fixed_form")) %>%
    dplyr::select(-dplyr::contains(match = "error_term"))

  .data
}

