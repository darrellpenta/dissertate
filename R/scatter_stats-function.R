#' Compute useful statistics for use with \code{\link[dissertate]{scatter}}.
#'
#'
#' Provides statistics that are used in \code{\link[dissertate]{scatter}} and passed to \code{\link[ggplot2]{ggplot2}}) to display model fit results and determin appropriate plot scales, etc.
#'
#' @param statdf The dataframe being plotted.
#' @param statx,staty  The (bare) names of the column in \code{statdf} to be used as the \code{x} and \code{y} in the scatterplot.
#' @param grp_by_form If the scatter plot will include faceting, a list of RHS formulas (see \code{\link[lazyeval]{f_rhs}}) to group \code{statdf} by; default = \code{NULL}. Ensures stats are calculated separately for each group.
#' @return A dataframe with model fit statistics and minimum and maximum values for \code{statx} and \code{staty}
#' @export

scatter_stats <-
  function(statdf, statx, staty, grp_by_form = NULL) {


  #create lm(y~x) form --------------
  lmform <-
    paste(lazyeval::expr_text(staty), lazyeval::expr_text(statx), sep = "~")

  lmform <-
    stats::as.formula(lmform)

  # model stats -------------------------------------------------------------
  if (is.null(grp_by_form)) {
    dat_grp <- statdf
  } else {
    grp <- lazyeval::as_f_list(grp_by_form)
    grp <- lapply(grp, as.formula)
    dat_grp <-
      dplyr::group_by_(.data = statdf, .dots = grp)
  }

  models <-
    dat_grp %>%
    dplyr::do(fit = lazyeval::f_eval(~ stats::lm(lazyeval::uqf(lmform), data = .)))
  si <-
    models %>%
    broom::tidy(fit, quick = TRUE) %>%
    tidyr::spread(term, estimate)
  colnames(si)[ncol(si) - 1] = "b"
  colnames(si)[ncol(si)] = "m"

  mods <-
    si %>%
    dplyr::ungroup() %>%
    dplyr::mutate(link = row.names(.)) %>%
    tbl_df()

  ys <-
    models %>%
    broom::glance(fit) %>%
    dplyr::rename(r = adj.r.squared, p = p.value) %>%
    dplyr::ungroup() %>%
    dplyr::select(r, p) %>%
    dplyr::mutate(link = row.names(.)) %>%
    dplyr::tbl_df()


  mods <-
    dplyr::left_join(x = mods, y = ys, by = "link") %>%
    dplyr::mutate(
      r = ifelse(is.nan(r) == TRUE, "", gsub(
        "^(-?)0.", "\\.", formattable::formattable(r, digits = 2)
      )),
      p = get_range_grt_than(p),
      r = ifelse(
        r == "",
        "",
        paste0(
          "$\\overset{\\textit{r^{2}}$=$ ",
          trimws(r),
          "}{\\textit{p}",
          trimws(p),
          "}$"
        )
      )
    )

  # minmax ------------------------------------------------------------------
  if (is.null(grp_by_form)) {
    cols =
      c(paste0(expr_text(statx)), paste0(expr_text(staty)))

    mnmxdf <-
      dplyr::summarise_at(.tbl = statdf,
                   .cols = cols,
                   .funs = c("max", "min")) %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::select(contains("max"), contains("min")) %>%
      sjmisc::remove_all_labels(.) %>%
      dplyr::as_data_frame()
    mnmxdf <- do.call("cbind", lapply(mnmxdf, data.frame))
    mnmx <-
      data.frame(
        xmx <- as.character(mnmxdf[, 1]),
        ymx <- as.character(mnmxdf[, 2]),
        xmn <- as.character(mnmxdf[, 3]),
        ymn <- as.character(mnmxdf[, 4])
      ) %>%
      dplyr::mutate(
        xmx <- as.numeric(xmx),
        ymx <- as.numeric(ymx),
        xmn <- as.numeric(xmn),
        ymn <- as.numeric(ymn)
      )
    mnmx
  } else {
    grp <- lazyeval::as_f_list(grp_by_form)
    grp <- lapply(grp, as.formula)
    cols <-
      c(paste0(expr_text(statx)), paste0(expr_text(staty)))

    mnmxdf <-
      dplyr::summarise_at(.tbl = dat_grp,
                   .cols = cols,
                   .funs = c("max", "min")) %>%
      dplyr::ungroup() %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::select(contains("max"), contains("min")) %>%
      sjmisc::remove_all_labels(.) %>%
      dplyr::as_data_frame()
    mnmxdf <- do.call("cbind", lapply(mnmxdf, data.frame))
    mnmx <-
      data.frame(
        xmx <- as.character(mnmxdf[, 1]),
        ymx <- as.character(mnmxdf[, 2]),
        xmn <- as.character(mnmxdf[, 3]),
        ymn <- as.character(mnmxdf[, 4])
      ) %>%
      dplyr::mutate(
        xmx <- as.numeric(xmx),
        ymx <- as.numeric(ymx),
        xmn <- as.numeric(xmn),
        ymn <- as.numeric(ymn)
      )
    mnmx
  }
  mods <-
    dplyr::bind_cols(mods, mnmx)
}
