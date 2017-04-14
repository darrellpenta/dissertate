# aov_sweeten <- function(results_df) {
#
#   results_df %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(main_number, set_number) %>%
#     dplyr::mutate_at(c("meansq","statistic"),"sweetstat") %>%
#     dplyr::mutate_at(c("p.value"),"sweetp") %>%
#     dplyr::select(main_number, set_number, set_id, label, group_factor,id,term, df, f = statistic, p = p.value, mse = meansq, stars)
#
# }
