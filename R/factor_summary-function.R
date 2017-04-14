#'  Summary stats for factors
#'
#'  @importFrom magrittr %>%
#'  @param data to summarize
#'  @param dv the dependent variable
#'  @param iv a vector with the names of the independent variables
#'  @param grp_factor a grouping factor
#'  @return a data.frame of summary stats
#'  @family factorial-design functions
#'  @rdname factor_summary
#'  @include summary_stats-function.R
#'  @include standard_error-function.R
#'  @include combine_factor_levels-function.R
#'  @include select_dots-function.R
#'  @export
#'
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
        summarise_stats(.data = ., .variable = dv)
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
                       dat = data,
                       dv_ = dv) {
         dat %>%
           ungroup() %>%
           summarise_stats(.data = ., .cols = iv_, .variable = dv_)

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
factor_summary <-
   Vectorize(
     FUN = function(data,
                    dv,
                    iv,
                    grp_factor)
       factor_summary(data,
                      dv,
                      iv,
                      grp_factor),
     vectorize.args = c("data", "dv", "grp_factor"),
     SIMPLIFY = FALSE,
     USE.NAMES = FALSE
   )
