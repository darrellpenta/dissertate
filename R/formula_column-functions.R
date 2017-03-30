# FUNCTIONS FOR COLUMNS ---------------------------------------------------

iv_between_col <-
  function(btw_data, btw_var) {
    btw_data %>%
      dplyr::select(dplyr::contains(match = btw_var)) %>%
      dplyr::select(btwn = dplyr::contains(match = "_len")) %>%
      dplyr::mutate(btwn = ifelse(btwn == min(.$btwn), "", paste0(btw_var))) %>%
      .$btwn
  }
aov_vars_col <-
  function(av_data, ...) {
    dots <- list(...)
    if (length(dots) == 0) {
      out <-
        colnames(dplyr::select(av_data, dplyr::contains(match = "_nm"))) %>%
        stringr::str_replace_all("_nm", "") %>%
        stringr::str_trim(., side = "both")
    } else{
      removecol <- paste0(dots)
      out <-
        dplyr::select(av_data, -starts_with(removecol)) %>%
        dplyr::select(dplyr::contains(match = "_nm")) %>%
        colnames() %>%
        stringr::str_replace_all("_nm", "") %>%
        stringr::str_trim(., side = "both")
    }
    
    out <-
      out %>%
      sapply(
        simplify = "vector",
        USE.NAMES = FALSE,
        FUN = function(v,
                       av_dat = av_data) {
          av_dat_out <-
            av_dat %>%
            dplyr::select_(.dots = select_f(item1 = paste0(v, "_len")))
          
          av_dat_out[[1]] <-
            ifelse(av_dat_out[[1]] == 1,
                   NA,
                   paste(v))
        }
      ) %>%
      apply(
        MARGIN = 1,
        FUN = function(x) {
          x <- paste(stats::na.omit(stringr::str_trim(x, side = "both")),
                     collapse = " * ")
        }
      )
    
  }
aov_select_col <-
  function(select_data, s_dep_var, s_grp_factor) {
    select_data_out <-
      c(
        colnames(dplyr::select(
          select_data, dplyr::contains(match = "_nm")
        )) %>% stringr::str_replace_all("_nm", "") %>% stringr::str_trim(., side = "both"),
        s_dep_var,
        s_grp_factor
      )
    select_data_out <-
      paste0("~list(",
             paste("~", select_data_out, collapse = ",",
                   sep = ""),
             ")")
    
  }
