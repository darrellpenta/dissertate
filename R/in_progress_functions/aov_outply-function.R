# aov_out. <-
#   function(.data,
#            dv,
#            iv,
#            grp_factor = NULL) {
#
#     .data %>%
#       dplyr::select_(.dots = select_f(item1=dv,iv,grp_factor)) %>%
#       dplyr::mutate_at(c(iv,grp_factor),"as.factor") %>%
#       stats::aov(lazyeval::f_eval(~ lazyeval::uqf(aov_form(dv = dv, iv = iv,grp_factor = grp_factor))), data = .)  %>%
#       aov_special_tidy()
#     }
#
#
# # aov_outply.default <-
# #   function(specs_df,source_dfs) {
# # aov_results_out <-
# #   specs_df %>%
# #   group_by(label) %>%
# #   do(., ldply(.data = .,.fun =  function(spec_row, src_df=source_dfs){
# #
# # src_dat <-
# #   source_dfs[spec_row$group_factor[1]] %>%
# #   dplyr::bind_rows()
# #
# # dat_filter <-
# #               list(paste0("~", spec_row$filter_formula)) %>%
# #               lazyeval::as_f_list(.) %>%
# #               lapply(stats::as.formula)
# # dat_select <-
# #   spec_row$select_formula %>%
# #   stats::as.formula(.)
# #
# # dat_anova <-
# #   source_anova <-
# #   spec_row$aov_formula %>%
# #   stats::as.formula(.)
# #
# # aov_out <-
# #   src_dat %>%
# #   dplyr::select_(.dots = f_eval(dat_select)) %>%
# #   mutate_all("as.character") %>%
# #   mutate_at(paste0(spec_row$dep_var),"as.numeric") %>%
# #   mutate_if(is.character,"as.factor") %>%
# #   dplyr::filter_(.dots = dat_filter) %>%
# #   stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(dat_anova)), data = .)  %>%
# #   re_tidy()
# #
# # })) %>%
# #   ungroup()
# # aov_results_out
# #   }
# #
#
# aov_outply.default <-
#   function(spec_row, src_df){
#
# src_dat <-
#   ifelse("list" %in% class(src_df),src_df[spec_row$group_factor] %>%
#            dplyr::bind_rows(),src_df)
# # if("list" %in% class(src_df)){}
# #   src_dat <-
# #     src_df[spec_row$group_factor] %>%
# #     dplyr::bind_rows()
#
#   dat_filter <-
#     list(paste0("~", spec_row$filter_formula)) %>%
#     lazyeval::as_f_list(.) %>%
#     lapply(stats::as.formula)
#   dat_select <-
#     spec_row$select_formula %>%
#     stats::as.formula(.)
#
#   anova_out <-
#     src_dat %>%
#     dplyr::select_(.dots = f_eval(dat_select)) %>%
#     dplyr::mutate_all("as.character") %>%
#     dplyr::mutate_at(paste0(spec_row$dep_var),"as.numeric") %>%
#     dplyr::mutate_if(is.character,"as.factor") %>%
#     dplyr::filter_(.dots = dat_filter) %>%
#     stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(spec_row$aov_formula %>%
#                                                    stats::as.formula(.))), data = .)  %>%
#     aov_special_tidy()
#   anova_out
# }
# # aov_outply <-
# #   function(spec_row, src_df){
# #
# #   src_dat <-src_df
# #
# #   dat_filter <-
# #     list(paste0("~", spec_row$filter_formula)) %>%
# #     lazyeval::as_f_list(.) %>%
# #     lapply(stats::as.formula)
# #   dat_select <-
# #     spec_row$select_formula %>%
# #     stats::as.formula(.)
# #
# #   anova_out <-
# #     src_dat %>%
# #     dplyr::select_(.dots = f_eval(dat_select)) %>%
# #     dplyr::mutate_all("as.character") %>%
# #     dplyr::mutate_at(paste0(spec_row$dep_var),"as.numeric") %>%
# #     dplyr::mutate_if(is.character,"as.factor") %>%
# #     dplyr::filter_(.dots = dat_filter) %>%
# #     stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(spec_row$aov_formula %>%
# #                                                    stats::as.formula(.))), data = .)  %>%
# #     aov_special_tidy()
# #   anova_out
# # }
#
#
#
# #
# #           source_anova <-
# #             spec$aov_formula %>%
# #             stats::as.formula(.)
# #
# #           s_df %>%
# #             dplyr::filter_(.dots = source_filter) %>%
# #             dplyr::select_(.dots = source_select) %>%
# #             dplyr::mutate_at(c(i_v, g_f), "as.factor") %>%
# #             stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(source_anova)), data = .)  %>%
# #             re_tidy()
# #
# #         }
# #       )
# #     out
# #   } else {
# #     out <-
# #       adply(
# #         specs_df,
# #         .margins = 1,
# #         .fun = function(spec,
# #                         s_df = source_df,
# #                         d_v = dv,
# #                         i_v = iv,
# #                         g_f = grp_factor) {
# #           source_filter <-
# #             list(paste0("~", spec$filter_formula)) %>%
# #             lazyeval::as_f_list(.) %>%
# #             lapply(stats::as.formula)
# #
# #           source_select <-
# #             select_f(d_v, i_v, g_f)
# #
# #           source_anova <-
# #             spec$aov_formula %>%
# #             stats::as.formula(.)
# #
# #           s_df %>%
# #             dplyr::filter_(.dots = source_filter) %>%
# #             dplyr::select_(.dots = source_select) %>%
# #             dplyr::mutate_at(c(i_v, g_f), "as.factor") %>%
# #             stats::aov(lazyeval::f_eval( ~ lazyeval::uqf(source_anova)), data = .)  %>%
# #             re_tidy()})
# #     out
# #   }
# #   out <-
# #     out %>%
# #     dplyr::mutate(aov_id = paste(grp_factor)) %>%
# #     # dplyr::mutate_if(is.factor, "as.character") %>%
# #     dplyr::select(aov_id, dplyr::everything())
# #   out
# # }
# #
# #
# #
