#' Helper functions for creating factor_filter_columns
#'
#' @param .data a df.factor.grid, or part of the grid
#' @param select_column the name of the column used to generate the ff_ column needed
#' @param ... additional arguments (ignore)
#' @rdname factor_filter_columns
#' @family factor_filter_columns functions

#' Create a ff_form_column
#' @rdname factor_filter_columns
#' @return a vector of filtering formulas
#' @family factor_filter_columns functions
#' @export
ff_forms_col <- function(.data, select_column, ...) {
  assertthat::validate_that(
    is.character(.data)
  )
  assertthat::validate_that(!(missing(select_column)))
  assertthat::validate_that(is.character(select_column))


  ff_form_col_out <-
    .data <- paste(select_column, paste0( "c(",
                                                 paste(
                                                   "'",
                                                   .data,
                                                   "'",
                                                   collapse = ",",
                                                   sep = ""
                                                 ),
                                                 ")"
    ),
    sep = " %in% ")
  ff_form_col_out

}

#' Create a "number_levels_column"
#'
#' @rdname factor_filter_columns
#' @return for <code>ff_number_levels_column</code>, a vector of length of .data
#' @family factor_filter_columns functions
#' @export
ff_number_levels_col <- function(.data, select_column, ...) {
  assertthat::validate_that(
    is.data.frame(.data) |
      tibble::is.tibble(.data),
    assertthat::has_attr(.data, "names")
  )

  if(missing(select_column)) {

    select_col <-
      "_len"
    select_col
  } else {
  assertthat::validate_that(is.character(select_column),
                            select_column %in% attr(.data, which = "names"))
 select_col <-
    select_column
  select_col
  }

  .data[grepl(paste0(select_col), names(.data), fixed = TRUE)] %>%
    dplyr::mutate_if(is.character, "as.integer") %>%
    apply(1, sum)
}



#' Create a ff_label_column
#' @rdname factor_filter_columns
#' @param match_pattern a pattern to match for breaking label vectors apart=(Default is "_nm")
#' @param replace_pattern a pattern to use for replacing matches
#' @return for <code>ff_label_column</code>, a vector of filtering formulas
#' @family factor_filter_columns functions
#' @export
ff_label_col <-
  function(.data, select_column, ..., match_pattern, replace_pattern){
assertthat::validate_that(
  is.data.frame(.data) |
    tibble::is.tibble(.data),
  assertthat::has_attr(.data, "names")
)

if(missing(select_column)) {

    select_col <-
      "_nm"
    select_col
  } else {
    assertthat::validate_that(is.character(select_column),
                              select_column %in% attr(.data, which = "names"))
    select_col <-
      select_column
    select_col
  }

if(missing(match_pattern)) {

    match_pat <-
      "([:]*)([a-z]{0,3})[a-z]*([:]*)"
    match_pat

  } else {
    assertthat::validate_that(is.character(match_pattern))
    match_pat <-
      match_pattern
    match_pat
  }
if(missing(replace_pattern)) {

    replace_pat <-
      "\\1\\2\\3"
    replace_pat

  } else {
    assertthat::validate_that(is.character(replace_pattern))
    replace_pat <-
      replace_pattern
    replace_pat
  }

  gsub_special <-
    function(data, pattern, replacement)  {
      data_out <-
        gsub(x = data,
             pattern = pattern,
             replacement = replacement,
             ignore.case = TRUE)
      data_out
    }

  data_out <-
  .data[grepl(paste0(select_col), names(.data), fixed = TRUE)]

  data_out <-
    dplyr::mutate_all(data_out,
                      gsub_special,
                      pattern=match_pat,
                      replacement=replace_pat)
  data_out <-
    tidyr::unite(data_out,
                 "label",
                 dplyr::everything(),
                 sep = "::")
  data_out<-
    stringr::str_c(data_out$label)

}
#' Create a ff_filter_f_column
#' @rdname factor_filter_columns
#' @return for <code>ff_number_levels_column</code>, a vector of length of .data
#' @export
ff_filter_f_col <-
  function(.data, select_column, ...){
assertthat::validate_that(
  is.data.frame(.data) |
    tibble::is.tibble(.data),
  assertthat::has_attr(.data, "names")
)

if(missing(select_column)) {

    select_col <-
      "_form.temp"
    select_col
  } else {
    assertthat::validate_that(is.character(select_column),
                              select_column %in% attr(.data, which = "names"))
    select_col <-
      select_column
    select_col
  }


  data_out <-
  .data[grepl(paste0(select_col),
              names(.data),
              fixed = TRUE)]

  data_out <-
    tidyr::unite(data_out,
                 "filter_form",
                 dplyr::contains(match = "_form.temp"),
                 sep = " & ")

  data_out<-
    stringr::str_c(data_out$filter_form)

    }



