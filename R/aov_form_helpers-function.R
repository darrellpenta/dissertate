#' AOV form helpers
#'
#' @param .data a  .dataframe
#' @param ... further arguments passed to or from other methods
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var the grouping factor
#' @param .btw_var an optional vector naming the between-subjects variable
#' @param .rm_col an optional vector naming columns to remove
#' @return a data.frame with a  class attribute for passing to additional
#' @include sweet_dots-function.R
#' @rdname aov_form_helpers
#' @export
#'
aov_formulate <- function(.data, ...) {
  UseMethod("aov_formulate", .data)
}

#' @rdname aov_form_helpers
#' @export

aov_formulate.between <- function(.data,
                                  ...,
                                  .dep_var,
                                  .grp_var,
                                  .btw_var) {
  af_dots <-
    pryr::named_dots(...)
  lapply(af_dots, eval, parent.frame())

  error_term <-
    ifelse(
      .data$aov_fixed_term == .data$iv_between,
      paste0("Error(", .grp_var, ") + ", .data$iv_between),
      ifelse(
        .data$iv_between == "",
        paste0(
          "Error(",
          paste(.grp_var,
                paste0("(",
                       .data$aov_error_denom, ")"),
                sep = "/"),
          ")",
          .data$iv_between
        ),
        paste0(
          "Error(",
          paste(.grp_var,
                paste0("(",
                       .data$aov_error_denom,
                       ")"),
                sep = "/"),
          ") + ",
          .data$iv_between
        )
      )
    )
  .data$error_term <-
    error_term

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

aov_formulate.within <- function(.data,
                                 ...,
                                 .dep_var,
                                 .grp_var) {
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
  .data$error_term <-
    error_term

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
aov_terms_col <- function(.data, .btw_var = FALSE, ...) {
  if (is.character(.btw_var)) {
    at_dots <-
      pryr::named_dots(...)
    lapply(at_dots, eval, parent.frame())
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

    class(.data) <- append(class(.data), "within")
    .data
  } else {
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

    class(.data) <- append(class(.data), "within")
    .data
  }
}

#' Between-subjects variable column
#'
#' @rdname aov_form_helpers
#' @export

iv_between_col <-
  function(.data, .btw_var, ...) {
    assertthat::validate_that(!(missing(.btw_var)), is.character(.btw_var))

    fc_dots <-
      pryr::named_dots(...)
    lapply(fc_dots, eval, parent.frame())

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

aov_vars_col <- function(.data, .rm_col = FALSE, ...) {
  fc_dots <-
    pryr::named_dots(...)

  lapply(fc_dots, eval, parent.frame())


  if (is.character(.rm_col) & !(isTRUE(.rm_col))) {
    remove_col <-
      paste0(.rm_col)
    sw <-
      dplyr::starts_with
    out <-
      dplyr::select(.data, -sw(remove_col)) %>%
      dplyr::select(dplyr::contains(match = "_nm")) %>%
      colnames() %>%
      stringr::str_replace_all("_nm", "")
    out <-
      stringr::str_trim(out, side = "both")
    out
  } else{
    out <-
      colnames(dplyr::select(.data, dplyr::contains(match = "_nm"))) %>%
      stringr::str_replace_all("_nm", "")
    out <-
      stringr::str_trim(out, side = "both")
    out
  }
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
                 paste(.v))
      }

    )
  out <- apply(
    out,
    MARGIN = 1,
    FUN = function(x) {
      x <- paste(stats::na.omit(stringr::str_trim(x, side = "both")),
                 collapse = " * ")
    }
  )
  out
}

#' Anova select column
#'
#' @rdname aov_form_helpers
#' @export
#'
aov_select_col <-
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
      paste0("~list(",
             paste("~", select_data_out, collapse = ",",
                   sep = ""),
             ")")


    .data$select_form <-
      select_data_out
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

  set_id <- paste(toupper(.grp_var), toupper(.dep_var), "_")

  .data$set_id <-
    set_id

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
    dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
    dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
    dplyr::select(-dplyr::contains(match = "_denom")) %>%
    dplyr::select(-dplyr::contains(match = "_between")) %>%
    dplyr::select(-dplyr::contains(match = "aov_fixed_form")) %>%
    dplyr::select(-dplyr::contains(match = "error_term"))

  .data
}
