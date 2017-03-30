factor_filter.default <-
  function(data) {
    data <-
      dplyr::bind_rows(data)

    iv <- c(paste(colnames(data)))


    levels_df <- #loop over each named independent variable
      lapply(iv, function(z,
                          d = data) {
        d_out <- #subset .data one variable at a time
          d %>%
          dplyr::select_(.dots = select_dots(item1 = z))

        d_newcols <- #renaming df allows for flexible expansion
          tibble::tibble("col1" = d_out[[1]])

        d_newcols_1 <-
          lapply(d_newcols$col1,
                 strsplit,
                 split = ":") %>%
          unlist(recursive = FALSE)

        d_out$len <-
          # Setting the length (of the vector) to keep or drop variable accordingly
          lapply(d_newcols_1,
                 length) %>%
          stringr::str_c()

        d_out$form <-
          # dynamic creation of filtering conditions for analyses
          lapply(d_newcols_1,
                 function(dvec) {
                   dvec <-
                     paste(z,
                           paste0("c(",
                                  paste(
                                    "'", dvec, "'", collapse = ",",
                                    sep = ""
                                  ), ")"),
                           sep = " %in% ")
                 }) %>%
          stringr::str_c()

        d_out <- #rename tiny data set with name of variable
          d_out %>%
          `names<-`(c(
            paste0(z, "_nm"),
            paste0(z, "_len"),
            paste0(z, "_form.temp")
          ))
      }) %>%
      as.data.frame(stringAsFactors = "FALSE")

    levels_df$sums <- # summing the no. of levels invovled in analyses
      levels_df %>%
      dplyr::select(dplyr::contains(match = "_len")) %>%
      dplyr::mutate_if(is.character, "as.integer") %>%
      apply(1, sum)

    levels_df$label <- #labelling to make printing easier
      levels_df %>%
      dplyr::select(dplyr::contains(match = "_nm")) %>%
      dplyr::mutate_all(function(x) {
        gsub(
          x = x,
          pattern = "([:]*)([a-z]{0,3})[a-z]*([:]*)",
          "\\1\\2\\3",
          ignore.case = TRUE
        )
      }) %>%
      tidyr::unite(label,
                   dplyr::everything(),
                   sep = "::") %>%
      .$label

    levels_df$filter_formula <-
      #combines filtering conditions for variables
      levels_df %>%
      dplyr::select(dplyr::contains(match = "_form.temp")) %>%
      tidyr::unite(filter_f,
                   dplyr::contains(match = "_form.temp"),
                   sep = " & ") %>%
      .$filter_f

    levels_df <-
      #orders the data from largest analyses to smallest, removes illogical groupings
      levels_df %>%
      dplyr::filter(!(sums %in% min(levels_df$sums))) %>%
      dplyr::arrange(dplyr::desc(sums))

    # levels_df <- #drops len (unneeded)
    #   levels_df %>%
    #   dplyr::select(-dplyr::contains(match = "_len")) %>%
    #   dplyr::select(-dplyr::contains(match = "_nm")) %>%
    #   dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
    #   dplyr::select(label,sums, dplyr::everything())

    levels_df

    # levels_df$iv_f <- #creates the term for the IVs in the anova
    #   ldply(iv, function(z,
    #                      ivar = levels_df) {
    #     ivar_out <-
    #       ivar %>%
    #       dplyr::select_(.dots = select_f(item1 = paste0(z, "_len")))
    #
    #     ivar_out[[1]] <-
    #       ifelse(ivar_out[[1]] == 1,
    #              NA_character_,
    #              paste(z))
    #   }) %>%
    #   apply(MARGIN = 2,
    #         paste,
    #         collapse = ":") %>%
    #   sapply(FUN =  gsub,
    #          pattern = "[:]{1}[NA]{2}|[NA]{2}[:]{1}",
    #          replacement = "") %>%
    #   tibble::as_tibble() %>%
    #   dplyr::mutate(iv_f = gsub(
    #     pattern = ":",
    #     " * ",
    #     fixed = TRUE,
    #     x = value
    #   )) %>%
    #   .$iv_f
    #

  }


factor_filter <-
  Vectorize(
    FUN = function(data)
      factor_filter.default(data),
    vectorize.args = c("data"),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
