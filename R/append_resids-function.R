#' Append residuals to a dataframe.
#'

#' \code{append_resids} returns a copy of dataframe with residual values from \code{\link[stats]{lm}} appended.
#'
#' @param dataframe A dataframe with at least two numeric or integer columns suitable for fitting with \code{\link[stats]{lm}}.
#' @param id.group  A list of one or more RHS formulas (see \code{\link[lazyeval]{f_rhs}}) naming the dataframe column(s) that will be used to create a unique row identifier for binding the column of residual values to the original dataframe: list(~col1,~col2); if NULL (default), assumes each row is unique. )
#' @param lm.form = A list of exactly two RHS formulas (see \code{\link[lazyeval]{f_rhs}}) naming the dataframe columns over which residulas will be computed, or else a formula to be passed to  \code{\link[stats]{lm}}.
#' @return A copy of the original dataframe with residual columns appended. When the \code{lm.form} arguments is a list of exactly two RHS formulas, column names are \code{col1.res.col2} & \code{col2.res.col2}; otherwise, the coloumn name is a reasonably interpretable but abbreviated version of the column names in the formula passed to \code{\link[stats]{lm}}, where the LHS and RHS are separate by \code{.res.}
#' @export


append_resids <-
  function(dataframe, id.group, lm.form) {
    # Evaluate id.group -------------------------------------------------------
    if (is.null(id.group)) {
      dataframe_1 <-
        dataframe %>%
        dplyr::mutate(id.group = as.numeric(row.names(.)))
    } else {
      if (is.list(id.group) == FALSE) {
        stop("'id.group' must be a list of one or more RHS funcions.")

      } else {
        # id.group.check <-
        #   id.group %>%
        #   lapply(lazyeval::uq)
        #
        # absent_cols <-
        #   id.group.check %in% colnames(dataframe)
        #
        # if (all(absent_cols) == FALSE) {
        #   stop(
        #     paste0(
        #       lazyeval::expr_label(dataframe),
        #       "does not have column(s) named:\"",
        #       id.group.check[which(absent_cols == FALSE)],
        #       "\""
        #     )
        #   )
        # } else {
        id.group.form <-
          lazyeval::as_f_list(id.group) %>%
          lapply(stats::as.formula)

        dataframe_1 <-
          dataframe

        dataframe_1$id.group <-
          as.numeric(interaction(
            dataframe_1 %>%
              dplyr::select_(.dots = id.group.form),
            drop =  FALSE
          ))
      }
    }
    # Evaluate lm.form  --------------------------------------------------------
    if (is.list(lm.form)) {
      y <-
        lm.form[[1]]

      x <-
        lm.form[[2]]

      # absent_xy <-
      #   c(lazyeval::uq(y), lazyeval::uq(x)) %in% colnames(dataframe)
      #
      # if (all(absent_xy) == FALSE) {
      #   stop(
      #     paste0(
      #       lazyeval::expr_label(dataframe),
      #       "does not have column(s) named:\"",
      #       absent_xy[which(absent_xy == FALSE)],
      #       "\""
      #     )
      #   )
      # } else {
      #   y <-
      #     lm.form[[1]]
      #
      #   x <-
      #     lm.form[[2]]


      yx <-
        paste(lazyeval::uq(y), "~", lazyeval::uq(x)) %>%
        stats::as.formula(.)
      xy <-
        paste(lazyeval::uq(x), "~", lazyeval::uq(y)) %>%
        stats::as.formula(.)

      col.select.form <-
        list( ~ id.group, lazyeval::uqf(y), lazyeval::uqf(x)) %>%
        lazyeval::as_f_list(.) %>%
        lapply(stats::as.formula)

      dataframe_2 <-
        dataframe_1 %>%
        dplyr::select_(.dots = col.select.form) %>%
        stats::na.omit() %>%
        unique()

      dataframe_2yx <-
        dataframe_2 %>%
        dplyr::do(broom::augment(stats::lm(lazyeval::uqf(yx), data = .))) %>%
        dplyr::select(`.resid`)

      colnames(dataframe_2yx) <-
        paste0(lazyeval::uq(y), ".res.", lazyeval::uq(x))

      dataframe_2xy <-
        dataframe_2 %>%
        dplyr::do(broom::augment(stats::lm(lazyeval::uqf(xy), data = .))) %>%
        dplyr::select(`.resid`)

      colnames(dataframe_2xy) <-
        paste0(lazyeval::uq(x), ".res.", lazyeval::uq(y))

      residuals.df <-
        dplyr::bind_cols(dataframe_2yx, dataframe_2xy)

      dataframe_2 <-
        dplyr::bind_cols(dataframe_2 %>% dplyr::select(id.group), residuals.df)

      dataframe.out <-
        dplyr::left_join(dataframe_1, dataframe_2, by = "id.group") %>%
        dplyr::select(-id.group)

      dataframe.out
      # }

      # } else {
      #   if (!(isTRUE(
      #     as.character(lazyeval::f_lhs(lazyeval::uqf(lm.form))) %in% colnames(dataframe_1)
      #   ))) {
      #     stop(
      #       paste0(
      #         "Bad LHS of formula: \"",
      #         lazyeval::f_lhs(lazyeval::uqf(lm.form)),
      #         "\" is not a column in the dataframe."
      #       )
      #     )

      # } else {
      #   # Check RHS of lm.form ----------------------------------------------------
      #   lm.rhs.names <-
      #     unlist(stringi::stri_split_boundaries(as.character(lazyeval::f_rhs(
      #       lazyeval::uqf(lm.form)
      #     ))))
      #   lm.rhs.names <-
      #     lm.rhs.names[which(lm.rhs.names > 1)]
      #
      #   if (all(lm.rhs.names %in% colnames(dataframe_1)) == FALSE) {
      #     stop(
      #       paste0(
      #         "Bad RHS of formula: The dataframe does not have column(s) name \"",
      #         lm.rhs.names[which(lm.rhs.names %in% colnames(dataframe_1) == FALSE)]        ,
      #         "\""
      #       )
      #     )
    } else {
      rhs.lm.form <-
        lapply(lm.rhs.names, function(x)
          paste("~", x))
      y <-
        paste("~", as.character(lazyeval::f_lhs(lm.form)))

      xy.form <-
        list(~ id.group, y[1], rhs.lm.form) %>%
        unlist(recursive = TRUE) %>%
        lazyeval::as_f_list(.) %>%
        lapply(stats::as.formula)

      dataframe_2 <-
        dataframe_1 %>%
        dplyr::select_(.dots = xy.form) %>%
        stats::na.omit() %>%
        unique()

      residuals.df <-
        dataframe_2 %>%
        dplyr::do(broom::augment(stats::lm(lazyeval::uqf(lm.form), data = .))) %>%
        dplyr::select(`.resid`)

      xcolname <-
        substr(as.character(lazyeval::f_lhs(lm.form)),
               start = 1,
               stop = 3)
      ycolname <-
        as.character(rhs.lm.form) %>%
        unlist() %>%
        gsub(pattern = "[^[:alnum:] ]", replacement = "") %>%
        abbreviate(minlength = 3, named = FALSE) %>%
        stringi::stri_flatten(str = ., collapse = ".")

      colnames(residuals.df) <-
        paste0(xcolname, ".res.", ycolname)

      dataframe_2 <-
        dplyr::bind_cols(dataframe_2 %>% dplyr::select(id.group), residuals.df)

      dataframe.out <-
        dplyr::left_join(dataframe_1, dataframe_2, by = "id.group") %>%
        dplyr::select(-id.group)

      dataframe.out
    }
  }
