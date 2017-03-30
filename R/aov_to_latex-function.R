# aov_to_latex <- function(aov_df) {
#   f_stat_combine <- function(aovdf) {
#     aovdf <-
#       aovdf %>% 
#     dplyr::ungroup() %>% 
#       tibble::as_tibble() %>% 
#       tidyr::unite(bind, main_number, set_number, set_id,label, group_factor,id, sep = "_", remove=TRUE) 
#     
#     aov_out <-
#       dplyr::left_join(
#         aovdf %>% 
#           dplyr::filter(term != "Residuals") %>% 
#           dplyr::select(bind,term,df, f = statistic,p = p.value, flabel),
#         aovdf %>% 
#           dplyr::filter(term == "Residuals") %>% 
#           dplyr::select(bind,r_df = df, mse = meansq),
#         by = "bind") %>% 
#       dplyr::mutate(stats = paste0(flabel,"(",df,",",r_df,") $=$ ",f,", \\ext{MS$_e$} $=$ ",mse,", \\ext{p}",ifelse(p == ".001",paste0(" < ",p),paste0(" $=$ ",p)))) %>% 
#       dplyr::select(term,stats) 
#     aov_out
#   }
#   
#     out_aov <-
#       aov_df %>% 
#     dplyr::ungroup() %>% 
#     dplyr::mutate_all("as.character") %>% 
#     dplyr::mutate(flabel = dplyr::case_when(.$group_factor == "subject" ~ "\\ext{F$_1$}",
#                                      .$group_factor == "item" ~ "\\ext{F$_2$}")) %>% 
#     dplyr::group_by(main_number,set_number,set_id,label,group_factor,id) %>% 
#     dplyr::do(f_stat_combine(aovdf = .)) %>% 
#     dplyr::mutate_at(c("main_number","set_number"),"as.numeric") %>% 
#       dplyr::ungroup()
#     
#     if( nrow(out_aov %>% ungroup() %>%  select(group_factor) %>% unique()) != 1){
#       
#       out_aov <-
#         out_aov %>% 
#         ungroup()
#       
#       sie %>% 
#         dplyr::select(group_factor) %>% 
#         unique() %>% 
#         lapply(sie %>% select(group_factor)), function(x, dat=sie){
#           x <-
#             filter(dat, group_factor == paste0(x))
#             
#         })
#       
#       
#     } else {
#       out_aov
#     }
# 
# #   
# #   if(isTRUE(ind_var_between)){
# # 
# #      
# #        plyr::ddply(.(sum_order, aov_id, result), function(section){
# #           
# #           dplyr::bind_cols(
# #             section %>%
# #               dplyr::slice(1) %>%
# #               dplyr::select(df, f = statistic,p = p.value, flabel),
# #             section %>%
# #               dplyr::slice(2) %>%
# #               dplyr::select(r_df = df, mse = meansq)
# #           ) %>% 
# #             dplyr::mutate(stats = paste0(flabel,"(",df,",",r_df,") $=$ ",f,", \\ext{MS$_e$} $=$ ",mse,", \\ext{p}",ifelse(p == ".001",paste0(" < ",p),paste0(" $=$ ",p)))) %>% 
# #             dplyr::select(stats) 
# #         }) %>%
# #         dplyr::select(-sum_order)
# #   }
# #   else{
# #   .data %>%
# #     group_by(main_number, set_number, set_id, label, group_factor)
# #     dplyr::mutate(flabel = 
# #                     dplyr::case_when(.$aov_id == "SUBJECT" ~ "\\ext{F$_1$}",
# #                                      .$aov_id == "ITEM" ~ "\\ext{F$_2$}")) %>% 
# #   plyr::ddply(.(sum_order, aov_id, result), function(section){
# # 
# #       dplyr::bind_cols(
# #         section %>%
# #           dplyr::slice(1) %>%
# #           dplyr::select(df, f = statistic,p = p.value, flabel),
# #         section %>%
# #           dplyr::slice(2) %>%
# #           dplyr::select(r_df = df, mse = meansq)
# #       ) %>% 
# #       dplyr::mutate(stats = paste0(flabel,"(",df,",",r_df,") $=$ ",f,", \\ext{MS$_e$} $=$ ",mse,", \\ext{p}",ifelse(p == ".001",paste0(" < ",p),paste0(" $=$ ",p)))) %>% 
# #         dplyr::select(stats) 
# #     }) %>%
# #   dplyr::select(-sum_order)
# # }
# }
# 
