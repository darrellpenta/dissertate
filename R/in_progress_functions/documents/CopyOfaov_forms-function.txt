#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for \code{\link[stats]{aov}} from a \code{filter.grid} dataframe returned from \code{\link{factor_filter}.
#'

#' @param data a \code{filter.grid} dataframe
#' @param
#' @param ... further arguments passed to or from other methods
#' @return a data.frame with a \code{filter.grid} class attribute for passing to additional \link{factorial-design functions}
#' @family factorial-design functions
#'
#' @export
#'
aov_forms <- function(data, ...) {
  UseMethod("aov_forms", data)
}



#' A vectorized vesion of the \code{factor_filter} function
#' @family factorial-design functions
#' @rdname factor_filter
#' @importFrom magrittr %>%



#' factor_filter.factor.grid <-
#'   function(data,
#'            trunc_label = TRUE,
#'            for_aov = TRUE,
#'            ...) {
#'     assertthat::validate_that(
#'       assertthat::has_attr(data, which = "class"),
#'       "factor.grid" %in% attr(data, which = "class")
#'     )
#'     options("stringsAsFactors" = FALSE)
#'     iv <-
#'       c(paste(colnames(data)))
#'
#'
#'
#'
#'
#' aov_formply.default <-  function(levels_df, dep_var, ind_var_btwn, grp_factor) {
#'
#'
#' # - -----------------------------------------------------------------------
#'
#'   use_btwn_var <-  # Is there a between-subject IV?
#'     ifelse(missing(ind_var_btwn),FALSE,TRUE)
#'
#'   levels_df <-
#'     levels_df %>%
#'     dplyr::bind_rows()
#'
#'
#' # Prepare between-IV columns for ANOVA formula ----------------------------
#'   if(isTRUE(use_btwn_var)) {
#'   levels_df$iv_between <-
#'     iv_between_col(btw_data = levels_df, btw_var = ind_var_btwn)
#'   levels_df$aov_vars_form <-
#'     aov_vars_col(av_data = levels_df)
#'   levels_df$aov_error_denom <-
#'     aov_vars_col(av_data = levels_df, ind_var_btwn)
#'
#'   levels_df
#'   } else{
#'
#'     levels_df$aov_vars_form <-
#'       aov_vars_col(av_data = levels_df)
#'     levels_df$aov_error_denom <-
#'       aov_vars_col(av_data = levels_df)
#'     levels_df
#'   }
#'
#'
#'
#' # Subset data to create terms for AOV
#' aov_create_form_dat <-
#'   if (isTRUE(use_btwn_var)) {
#'     levels_df  %>%
#'       dplyr::bind_rows() %>%
#'       dplyr::select_(.dots =
#'                        select_f(item1 = sapply(
#'                          X = c("iv_between",
#'                                "aov_vars_form",
#'                                "aov_error_denom"),
#'                          USE.NAMES = FALSE,
#'                          paste,
#'                          collapse = ","
#'                        )))
#'
#'   } else {
#'     levels_df  %>%
#'       dplyr::bind_rows() %>%
#'       dplyr::select_(.dots =
#'                        select_f(item1 = sapply(
#'                          X = c("aov_vars_form",
#'                                "aov_error_denom"),
#'                          USE.NAMES = FALSE,
#'                          paste,
#'                          collapse = ","
#'                        )))
#'   }
#'
#' # Create AOV formula
#' levels_df$aov_formula <-
#'    aov_create_form_dat %>%
#'        plyr::adply(.margins = 1,
#'              .fun =
#'                function(form_row,
#'                         dep_var_ = dep_var,
#'                         err_btwn = use_btwn_var,
#'                         grp_ = grp_factor)
#'   { # Create Error term
#'         error_f <-
#'                    if (isTRUE(err_btwn))
#'                    {
#'                      ifelse(
#'                        form_row$aov_vars_form == form_row$iv_between,
#'                        paste0("Error(", grp_, ") + ", form_row$iv_between),
#'                        ifelse(
#'                          form_row$iv_between == "",
#'                          paste0(
#'                            "Error(",
#'                            paste(grp_, paste0("(", form_row$aov_error_denom, ")"), sep = "/"),
#'                            ")",
#'                            form_row$iv_between
#'                          ),
#'                          paste0(
#'                            "Error(",
#'                            paste(grp_, paste0("(", form_row$aov_error_denom, ")"), sep = "/"),
#'                            ") + ",
#'                            form_row$iv_between
#'                          )
#'                        )
#'                      )
#'                    } else
#'                    {
#'                      paste0("Error(",
#'                             paste(grp_,
#'                                   paste0("(", form_row$aov_error_denom, ")"),
#'                                   sep = "/"),
#'                             ")")
#'                    }
#'
#'       aov_f_out <-
#'         paste(paste(dep_var_,
#'               form_row$aov_vars_form,
#'                              sep = " ~ "),
#'                        error_f,
#'                        sep = " + ")
#' aov_f_out[[1]]},.expand = FALSE,.id = NULL) %>%
#'   unlist(
#'     recursive = FALSE,
#'     use.names = FALSE)
#'
#'
#' levels_df$select_formula <-
#'   aov_select_col(select_data = levels_df,s_dep_var = dep_var,s_grp_factor = grp_factor)
#'
#'
#' # Create index xolumn -----------------------------------------------------
#' levels_df <-  tibble::rownames_to_column(df = levels_df,var = "set_number")
#' levels_df <-
#'   levels_df %>%
#'   dplyr::mutate(group_factor = paste0(grp_factor),
#'                 aid1 = toupper(group_factor),
#'                 aid2 = toupper(paste0(dep_var))
#'                 ) %>%
#'   tidyr::unite(set_id, aid1, aid2, sep="_", remove= TRUE)
#'
#'
#' levels_df <- #drops len (unneeded)
#'   levels_df %>%
#'   dplyr::select(-dplyr::contains(match = "_len")) %>%
#'   dplyr::select(-dplyr::contains(match = "_nm")) %>%
#'   dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
#'   dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
#'   dplyr::select(-dplyr::contains(match = "_denom")) %>%
#'   dplyr::select(-dplyr::contains(match = "index")) %>%
#'   dplyr::select(-dplyr::contains(match = "sums")) %>%
#'   dplyr::select(set_number, label, dplyr::everything()) %>%
#'   dplyr::mutate(dep_var = paste0(dep_var))
#'
#' levels_df <-
#' if (isTRUE(use_btwn_var)) {
#'   levels_df %>%
#'     dplyr::select(-dplyr::contains(match = "_between"))
#' }else{
#'   levels_df
#' }
#'
#' levels_df
#'
#' }
#'
#'
#' aov_formply.vectorized <-
#'   Vectorize(
#'     FUN = function(levels_df, dep_var, ind_var_btwn, grp_factor)
#'       aov_formply.default(levels_df, dep_var, ind_var_btwn, grp_factor),
#'     vectorize.args = c("levels_df","dep_var","grp_factor"),
#'     SIMPLIFY = FALSE,
#'     USE.NAMES = FALSE
#'   )
#'
#'
#' aov_formply <-
#'   function(levels_df,
#'            dep_var,
#'            grp_factor,
#'            ...) {
#'
#' dots <- list(...)
#'
#' if(length(dots) == 0){
#'     out <-
#'       aov_formply.vectorized(
#'         levels_df = levels_df,
#'         dep_var = dep_var,
#'         grp_factor = grp_factor
#'       )
#'     out
#' }else{
#'
#'   ind_var_btwn = paste0(dots)
#'   out<-
#'   aov_formply.vectorized(
#'       levels_df = levels_df,
#'       dep_var = dep_var,
#'       ind_var_btwn = ind_var_btwn,
#'       grp_factor = grp_factor
#'     )
#'   out
#' }
#'     out <-
#'       out %>%
#'       plyr::ldply(bind_rows, .id = "dep_var") %>%
#'       dplyr::mutate_if(is.factor, "as.character")
#'
#'     out$main_number <- as.character(as.numeric(factor(out$dep_var, levels = unique(out$dep_var))))
#'     out<-
#'       out %>%
#'       dplyr::select(main_number, set_number, set_id, dplyr::everything()) %>%
#'       dplyr::mutate_at(c("main_number", "set_number"), "as.numeric")
#'     out
#'   }
#'
#'
#'
#'
#'
#' # aov_formply.default <-  function(levels_df, dep_var, ind_var_btwn, grp_factor) {
#' #
#' #
#' # # - -----------------------------------------------------------------------
#' #
#' #   use_btwn_var <-  # Is there a between-subject IV?
#' #     ifelse(missing(ind_var_btwn),FALSE,TRUE)
#' #
#' #   levels_df <-
#' #     levels_df %>%
#' #     dplyr::bind_rows()
#' #
#' #
#' # # Prepare between-IV columns for ANOVA formula ----------------------------
#' #   if(isTRUE(use_btwn_var)) {
#' #   levels_df$iv_between <-
#' #     iv_between_col(btw_data = levels_df, btw_var = ind_var_btwn)
#' #   levels_df$aov_vars_form <-
#' #     aov_vars_col(av_data = levels_df)
#' #   levels_df$aov_error_denom <-
#' #     aov_vars_col(av_data = levels_df, ind_var_btwn)
#' #
#' #   levels_df
#' #   } else{
#' #
#' #     levels_df$aov_vars_form <-
#' #       aov_vars_col(av_data = levels_df)
#' #     levels_df$aov_error_denom <-
#' #       aov_vars_col(av_data = levels_df)
#' #     levels_df
#' #   }
#' #
#' #
#' #
#' # # Subset data to create terms for AOV
#' # aov_create_form_dat <-
#' #   if (isTRUE(use_btwn_var)) {
#' #     levels_df  %>%
#' #       dplyr::bind_rows() %>%
#' #       dplyr::select_(.dots =
#' #                        select_f(item1 = sapply(
#' #                          X = c("iv_between",
#' #                                "aov_vars_form",
#' #                                "aov_error_denom"),
#' #                          USE.NAMES = FALSE,
#' #                          paste,
#' #                          collapse = ","
#' #                        )))
#' #
#' #   } else {
#' #     levels_df  %>%
#' #       dplyr::bind_rows() %>%
#' #       dplyr::select_(.dots =
#' #                        select_f(item1 = sapply(
#' #                          X = c("aov_vars_form",
#' #                                "aov_error_denom"),
#' #                          USE.NAMES = FALSE,
#' #                          paste,
#' #                          collapse = ","
#' #                        )))
#' #   }
#' #
#' # # Create AOV formula
#' # levels_df$aov_formula <-
#' #    aov_create_form_dat %>%
#' #        plyr::adply(.margins = 1,
#' #              .fun =
#' #                function(form_row,
#' #                         dep_var_ = dep_var,
#' #                         err_btwn = use_btwn_var,
#' #                         grp_ = grp_factor)
#' #   { # Create Error term
#' #         error_f <-
#' #                    if (isTRUE(err_btwn))
#' #                    {
#' #                      ifelse(
#' #                        form_row$aov_vars_form == form_row$iv_between,
#' #                        paste0("Error(", grp_, ") + ", form_row$iv_between),
#' #                        ifelse(
#' #                          form_row$iv_between == "",
#' #                          paste0(
#' #                            "Error(",
#' #                            paste(grp_, paste0("(", form_row$aov_error_denom, ")"), sep = "/"),
#' #                            ")",
#' #                            form_row$iv_between
#' #                          ),
#' #                          paste0(
#' #                            "Error(",
#' #                            paste(grp_, paste0("(", form_row$aov_error_denom, ")"), sep = "/"),
#' #                            ") + ",
#' #                            form_row$iv_between
#' #                          )
#' #                        )
#' #                      )
#' #                    } else
#' #                    {
#' #                      paste0("Error(",
#' #                             paste(grp_,
#' #                                   paste0("(", form_row$aov_error_denom, ")"),
#' #                                   sep = "/"),
#' #                             ")")
#' #                    }
#' #
#' #       aov_f_out <-
#' #         paste(paste(dep_var_,
#' #               form_row$aov_vars_form,
#' #                              sep = " ~ "),
#' #                        error_f,
#' #                        sep = " + ")
#' # aov_f_out[[1]]},.expand = FALSE,.id = NULL) %>%
#' #   unlist(
#' #     recursive = FALSE,
#' #     use.names = FALSE)
#' #
#' #
#' # levels_df$select_formula <-
#' #   aov_select_col(select_data = levels_df,s_dep_var = dep_var,s_grp_factor = grp_factor)
#' #
#' #
#' # # Create index xolumn -----------------------------------------------------
#' # levels_df <-  tibble::rownames_to_column(df = levels_df,var = "set_number")
#' # levels_df <-
#' #   levels_df %>%
#' #   dplyr::mutate(group_factor = paste0(grp_factor),
#' #                 aid1 = toupper(group_factor),
#' #                 aid2 = toupper(paste0(dep_var))
#' #                 ) %>%
#' #   tidyr::unite(set_id, aid1, aid2, sep="_", remove= TRUE)
#' #
#' #
#' # levels_df <- #drops len (unneeded)
#' #   levels_df %>%
#' #   dplyr::select(-dplyr::contains(match = "_len")) %>%
#' #   dplyr::select(-dplyr::contains(match = "_nm")) %>%
#' #   dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
#' #   dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
#' #   dplyr::select(-dplyr::contains(match = "_denom")) %>%
#' #   dplyr::select(-dplyr::contains(match = "index")) %>%
#' #   dplyr::select(-dplyr::contains(match = "sums")) %>%
#' #   dplyr::select(set_number, label, dplyr::everything()) %>%
#' #   dplyr::mutate(dep_var = paste0(dep_var))
#' #
#' # levels_df <-
#' # if (isTRUE(use_btwn_var)) {
#' #   levels_df %>%
#' #     dplyr::select(-dplyr::contains(match = "_between"))
#' # }else{
#' #   levels_df
#' # }
#' #
#' # levels_df
#' #
#' # }
#' #
#' #
#' # aov_formply.vectorized <-
#' #   Vectorize(
#' #     FUN = function(levels_df, dep_var, ind_var_btwn, grp_factor)
#' #       aov_formply.default(levels_df, dep_var, ind_var_btwn, grp_factor),
#' #     vectorize.args = c("levels_df","dep_var","grp_factor"),
#' #     SIMPLIFY = FALSE,
#' #     USE.NAMES = FALSE
#' #   )
#' #
#' #
#' # aov_formply <-
#' #   function(levels_df,
#' #            dep_var,
#' #            grp_factor,
#' #            ...) {
#' #
#' # dots <- list(...)
#' #
#' # if(length(dots) == 0){
#' #     out <-
#' #       aov_formply.vectorized(
#' #         levels_df = levels_df,
#' #         dep_var = dep_var,
#' #         grp_factor = grp_factor
#' #       )
#' #     out
#' # }else{
#' #
#' #   ind_var_btwn = paste0(dots)
#' #   out<-
#' #   aov_formply.vectorized(
#' #       levels_df = levels_df,
#' #       dep_var = dep_var,
#' #       ind_var_btwn = ind_var_btwn,
#' #       grp_factor = grp_factor
#' #     )
#' #   out
#' # }
#' #     out <-
#' #       out %>%
#' #       plyr::ldply(bind_rows, .id = "dep_var") %>%
#' #       dplyr::mutate_if(is.factor, "as.character")
#' #
#' #     out$main_number <- as.character(as.numeric(factor(out$dep_var, levels = unique(out$dep_var))))
#' #     out<-
#' #       out %>%
#' #       dplyr::select(main_number, set_number, set_id, dplyr::everything()) %>%
#' #       dplyr::mutate_at(c("main_number", "set_number"), "as.numeric")
#' #     out
#' #   }
#' #
#' #
#' #
#' #
#' #
#
