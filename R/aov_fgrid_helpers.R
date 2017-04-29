#' AOV form helpers
#'
#' @param .data a  .dataframe
#' @param .dep_var a vector naming the depend. var.
#' @param .grp_var the grouping factor
#' @param .btw_var an optional vector naming the between-subjects variable
#' @param ... further arguments passed to or from other methods
#' @return a data.frame with a  class attribute for passing to additional
#' @include sweet_dots-function.R
#' @rdname aov_form_helpers
#' @export
#'
aov_formulate <- function(.data,
                          .dep_var,
                          .grp_var,
                          .btw_var = FALSE,
                          ...) {
  af_dots <-
    pryr::named_dots(...)
  lapply(af_dots, eval, parent.frame())
if(is.character(.btw_var)){
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
} else if(missing(.btw_var) | !(isTRUE(.btw_var))) {

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
}}



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

    .data
  } else if(missing(.btw_var) | !(isTRUE(.btw_var))){
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

aov_vars_col <- function(.data, ...) {
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
                 paste0(.v))
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

    d <-
      .data[grepl(paste0("_sel.temp"), names(.data), fixed = TRUE)]

    d$dep_var <- paste0(.dep_var)
    d$grp_var <- paste0(.grp_var)


    d[] <-
      lapply(d[], function(x) x<- ifelse(is.na(x),NA,paste0("~",x)))

    d$sform <-
      apply(d,1,
            function(x) {
              str_c(x[!is.na(x)], collapse = ",")})

    d$sform <-
      d$sform <- apply(d["sform"], 1, function(x) paste0('list(',x,')'))

    .data$select_form <-
      d$sform
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
      .data[grepl(paste0("_sel.temp"), names(.data), fixed = TRUE)]

   d$grp_var <- paste0(.grp_var)


  d[] <-
    lapply(d[], function(x) x<- ifelse(is.na(x),NA,paste0(x)))

  d$gbform <-
    apply(d,1,
          function(x) {
            str_c(x[!is.na(x)], collapse = ",")})

  d$gbform <- paste0("dplyr::group_by(",gbfpr,")")
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
    dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
    dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
    dplyr::select(-dplyr::contains(match = "_sel.temp")) %>%
    dplyr::select(-dplyr::contains(match = "_denom")) %>%
    dplyr::select(-dplyr::contains(match = "aov_fixed_form")) %>%
    dplyr::select(-dplyr::contains(match = "error_term"))

  .data
}

