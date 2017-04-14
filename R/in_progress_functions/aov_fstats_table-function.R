# aov_fstats_table <-
#   function(aov_df,
#            stat_wrap = c("Latex", "text"),
#            fs = TRUE) {
#     if (missing(stat_wrap)) {
#       stat_wrap = "Latex"
#       stat_wrap
#     }
#
#     if (missing(fs)) {
#       fs = TRUE
#     }
#
#     if (match.arg(stat_wrap) == "Latex") {
#       stat_list <- list(
#         "eq" = paste0(" $=$ "),
#         "less" = paste0(" < "),
#         "f" = paste0("\\ext{F}"),
#         "f1" = paste0("\\ext{F$_1$}"),
#         "f2" = paste0("\\ext{F$_2$}"),
#         "mse" = paste0("\\ext{MS$_e$} $=$ "),
#         "p" = paste0("\\ext{p}")
#       )
#       stat_list
#     } else {
#       stat_list <- list(
#         "eq" = paste0(" = "),
#         "less" = paste0(" < "),
#         "f" = paste0("F"),
#         "f1" = paste0("F1"),
#         "f2" = paste0("F2"),
#         "mse" = paste0("MSe = "),
#         "p" = paste0("p")
#       )
#       stat_list
#     }
#
#
#     out_aov_ <-
#       aov_df %>%
#       dplyr::ungroup() %>%
#       dplyr::mutate_all("as.character")
#
#     if (isTRUE(fs)) {
#       out_aov_ <-
#         out_aov_ %>%
#         dplyr::mutate(
#           flabel = dplyr::case_when(
#             .$group_factor == "subject" ~ stat_list["f1"][[1]],
#             .$group_factor == "item" ~ stat_list["f2"][[1]]
#           )
#         )
#       out_aov_
#     } else {
#       out_aov_ <-
#         out_aov_ %>%
#         dplyr::mutate(flabel = stat_list["f"][[1]])
#       out_aov_
#     }
#
#
#     out_aov_ <-
#       out_aov_ %>%
#       dplyr::group_by(main_number, set_number, set_id, label, group_factor, id) %>%
#       dplyr::do(aov_fstats_combine(aovdf = ., stat_ls = stat_list)) %>%
#       dplyr::mutate_at(c("main_number", "set_number"), "as.numeric") %>%
#       dplyr::ungroup()
#
#     out_aov_
#
#   }
