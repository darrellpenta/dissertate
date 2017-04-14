# aov_special_tidy <-
#   function(anova_results) {
#     if (class(anova_results)[1] != "aovlist") {
#       stop(paste(lazyeval::expr_label(aov_list), "is not an AOV list", sep = " "))
#     } else{
#       if (names(anova_results[1]) == "(Intercept)") {
#         aov_res <- anova_results[-1]
#         aov_res
#       } else{
#         aov_res <- anova_results
#         aov_res
#       }
#       aov_res_id <-
#         aov_res %>%
#         lapply(FUN = broom::tidy) %>%
#         lapply(dplyr::bind_rows) %>%
#         plyr::ldply(.fun = dplyr::bind_rows) %>%
#         tibble::as_tibble() %>%
#         dplyr::rename(id = .id) %>%
#         # dplyr::mutate_if(is.numeric,"apa_stat") %>%
#         dplyr::mutate(
#           stars = p_stars(p.value),
#           meansq = as.numeric(meansq),
#           statistic = as.numeric(statistic),
#           p.value = as.numeric(p.value)
#         ) %>%
#         dplyr::select(-sumsq)
#       aov_res_id
#     }
#   }
