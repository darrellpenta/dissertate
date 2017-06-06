#' AOV form helpers
#'
#' @param .data a  .dataframe
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var the grouping factor
#' @param .btw_var an optional between-subjects variable
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

  if(methods::hasArg(.btw_var)) {
  error_term <-
    ifelse(
      .data$aov_fixed_form == .data$iv_between,
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
          .data$iv_between)))
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
} else {
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
  }



#' @rdname aov_form_helpers
#' @export
aov_terms_col <- function(.data, ...) {

  if (methods::hasArg(.btw_var)) {

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

    .data

    } else {
    terms_cols <-
      .data %>%
      dplyr::select_(.dots =
                       sweet_dots(sapply(
                         X = c("aov_fixed_form",
                               "aov_error_denom"),
                         USE.NAMES = FALSE,
                         paste,
                         collapse = ","
                       )))
    .data <-
      dplyr::bind_cols(.data, terms_cols)

  }
}

#' Between-subjects variable column
#'
#' @rdname aov_form_helpers
#' @export

iv_between_col <-
  function(.data, .btw_var, ...) {
    assertthat::validate_that(!(missing(.btw_var)), is.character(.btw_var))

    dots <-
      pryr::named_dots(...)
    lapply(dots, eval, parent.frame())

    btw_var <- paste0(.btw_var)

    data_btw <-
      .data %>%
      dplyr::select(dplyr::contains(match = btw_var)) %>%
      dplyr::select(btw = dplyr::contains(match = "_len"))

    data_btw$btw <-
      ifelse(data_btw$btw == min(data_btw$btw), "", paste0(btw_var))

    data_btw$btw
  }

#' Anova variables column
#'
#' @rdname aov_form_helpers
#' @export

aov_vars_col <- function(.data, ...) {

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

