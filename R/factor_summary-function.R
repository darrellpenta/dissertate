#' Summary stats for factors
#'
#' @importFrom magrittr %>%
#' @param data to summarize
#' @param dv the dependent variable
#' @param iv a vector wiht the names of the independent variables
#' @param grp_factor a grouping factor
#' @return a data.frame of summary stats
#' @family factorial-design functions
#' @rdname factor_summary
#' @include sweetstat-function.R
#' @include standard_error-function.R
#' @include combine_factor_levels-function.R
#' @include select_dots-function.R
#' @export

factor_summary <- function(data,
                       dv,
                       iv,
                       grp_factor) {
  assertthat::validate_that(is.character(iv))
  table_len = length(iv) + 4
  dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble("data" = grp_factor),
      data %>%
        ungroup(data) %>%
        dplyr::summarise_at(
          .cols = paste0(dv),
          .funs = dplyr::funs(
            N = length,
            M = mean,
            SD = sd,
            SE = standard_error
          )
        ) %>%
        dplyr::mutate_if(is.double, sweetstat)
    ),
    combine_factor_levels(
      tibble::tibble("data" = iv),
      m = length(iv):1,
      simplify = FALSE,
      byrow = TRUE
    ) %>%
      unlist(recursive = FALSE,
             use.names = FALSE) %>%
      lapply(function(iv_,
                      .dat = data,
                      table_len = table_len) {
        data %>%
          ungroup() %>%
          dplyr::group_by_(.dots = select_dots(iv_)) %>%
          dplyr::summarise_at(
            .cols = paste0(dv),
            .funs =
              dplyr::funs(
                N = length,
                M = mean,
                SD = sd,
                SE = standard_error
              )
          ) %>%
          dplyr::mutate_if(is.double, sweetstat) %>%
          as.data.frame(ungroup(.),
                        stringsAsFactors = FALSE)
      }) %>%
      plyr::ldply(function(x,
                           t_len = table_len) {
        if (ncol(x) != 1) {
          x <-
            x %>%
            tidyr::unite(col = data,
                         -N,
                         -M,
                         -SD,
                         -SE,
                         sep = ":",
                         remove = TRUE)
        } else {
          x <-
            x %>%
            tidyr::unite(col = data,
                         -N,
                         -M,
                         -SD,
                         -SE,
                         sep = "",
                         remove = TRUE)
        }
      })
  ) %>%
    dplyr::arrange(dplyr::desc(N))
}

