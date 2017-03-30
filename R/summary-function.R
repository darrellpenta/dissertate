factor_sum <- function(.data,
                       dv,
                       iv,
                       grp_factor) {
  table_len = length(iv) + 4
  dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble("data" = grp_factor),
      .data %>%
        dplyr::summarise_at(
          .cols = "error",
          .funs = dplyr::funs(
            N = length,
            M = mean,
            SD = sd,
            SE = standard_error
          )
        ) %>%
        dplyr::mutate_if(is.double, sweetstat)
    ),
    combn_levels(
      tibble::tibble("data" = iv),
      m = length(iv):1,
      simplify = FALSE,
      byrow = TRUE
    ) %>%
      unlist(recursive = FALSE,
             use.names = FALSE) %>%
      lapply(function(iv_,
                      .dat = .data,
                      table_len = table_len) {
        .data %>%
          dplyr::group_by_(.dots = select_f(iv_)) %>%
          dplyr::summarise_at(
            .cols = "error",
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

