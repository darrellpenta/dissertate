#' A wrapper for a \code{ggplot2} scatterplot with a custom minimal theme.
#'
#'
#' \code{scatter} streamlines the process of creating a scatterplot in \code{ggplot2}, with some flexibilty for faceting (see \code{\link[ggplot2]{ggplot2}}).
#'
#' @param df The dataframe being plotted.
#' @param x,y  The (bare) names of the column to be used as the \code{x} and \code{y} in the scatterplot .
#' @param line_col The color of the \code{\link[ggplot2]{geom_smooth}} line; default is "#5B1A18" from the "GrandBudapest" palette in \link[https://github.com/karthik/wesanderson]{wes_palettes}.
#' @param fill_col The color of the \code{\link[ggplot2]{geom_smooth}} fill; default is "#FD6467" from the "GrandBudapest" palette in \link[https://github.com/karthik/wesanderson]{wes_palettes}.
#' @param facet_form A quoted string to be used as formula for facetting the figures, as, for example "factor1+factor2"; default = \code{NULL}.
#' @param grp_by If the scatter plot will include faceting, a list of RHS formulas (see \code{\link[lazyeval]{f_rhs}}) to be passed to \code{\link[disseRtate]{scatter_stats}}; default = \code{NULL}. Ensures stats are calculated separately for each group.
#' @param add_stats Logical; Add model fit info to the figure? Default = \code{TRUE}.
#' @return A ggplot2 object.
#' @export



scatter <-
  function(df,
           x,
           y,
           line_col = "#5B1A18",
           fill_col = "#FD6467",
           facet_form = NULL,
           grp_by    = NULL,
           add_stats = TRUE) {

    theme_poster <-
      ggplot2::theme(
        plot.margin =
          unit(c(.25, .25, .25, .25), "cm"),
        panel.grid.major =
          element_line(size = .5, color = "grey"),
        text =
          element_text(
            size = 18,
            family = "Helvetica",
            color = "#000000"
          ),
        #increase size of axis lines
        axis.line =
          element_line(size = .7, color = "#000000"),
        axis.text.x =
          element_text(
            size = 18,
            family = "Helvetica",
            color = "#000000"
          ),
        axis.text.y =
          element_text(
            size = 18,
            family = "Helvetica",
            color = "#000000"
          ),
        axis.title.y =
          element_text(
            size = 22,
            margin = margin(0, 15, 0, 0),
            family = "Helvetica",
            color = "#000000"
          ),
        axis.title.x =
          element_text(
            size = 22,
            margin = margin(15, 0, 0, 0),
            family = "Helvetica",
            color = "#000000"
          ),
        strip.text =
          element_text(
            face = "bold",
            size = 18,
            colour = "#000000",
            vjust = -12,
            family = "Helvetica"
          )
      )



    # create X and Y axis labels
    x_name <- paste("~", lazyeval::expr_text(x))
    x_name <- stats::as.formula(x_name)
    y_name <- paste("~", lazyeval::expr_text(y))
    y_name <- as.formula(y_name)
    xlb    <- labelled_x(xdata = df, xname = x_name)
    ylb    <- labelled_y(ydata = df, yname = y_name)

    yvar <- substitute(y)
    xvar <-  substitute(x)
    a <-
      ggplot2::ggplot(df,
                      eval(substitute(aes(x, y), list(
                        x = xvar, y = yvar
                      ))))  +
      ggplot2::geom_point(shape = 1) +
      ggplot2::xlab(xlb) +
      ggplot2::ylab(ylb) +
      ggplot2::theme_classic() +
      theme_poster

    a <-
      a +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        size = 1.5,
        fill = fill_col,
        color = line_col,
        fullrange = TRUE,
        alpha = .5
      )


    if (is.null(facet_form)) {
      stats = scatter_stats(statdf = df,
                            statx = x,
                            staty = y)

      xmx <- stats$xmx[1]
      ymx <- stats$ymx[1]
      xmn <- stats$xmn[1]
      ymn <- stats$ymn[1]

      if (ymn > 0 & xmn > 0) {
        a <-
          a +
          ggplot2::scale_x_continuous(breaks = seq(1, 7, 1),
                                      expand = c(0.025, 0)) +
          ggplot2::scale_y_continuous(breaks = seq(1, 7, 1),
                                      expand = c(0.025, 0)) +
          ggplot2::coord_fixed(ratio = 1,
                               xlim = c(1, 7),
                               ylim = c(1, 7))

        if (isTRUE(add_stats)) {
          a <-
            a +
            ggplot2::annotate(
              "text",
              x = 1,
              y = 7,
              label = paste(latex2exp::TeX(stats$r[1])),
              vjust = 1,
              hjust = 0,
              parse = TRUE,
              fontface = "bold",
              family = "Helvetica",
              size = 6,
              color = "black"
            )
        } else {
          a <- a
        }





      } else if (ymn == 0 & xmn >= 1) {
        b <- round(stats$b[1], digits = 1)
        m <- round(stats$m[1], digits = 1)
        y_lo <- b
        y_hi <- (m * 7) + b
        xy_ratio <- abs(1 / m + b) / 1

        a <-
          a +
          ggplot2::scale_x_continuous(breaks = seq(1, 7, 1),
                                      expand = c(0.025, 0)) +
          ggplot2::scale_y_continuous(breaks = round(seq(y_lo, y_hi, length.out = 7), 2),
                                      expand = c(0.025, 0)) +
          ggplot2::coord_fixed(
            ratio = xy_ratio,
            xlim = c(1, 7),
            ylim = c(y_lo, y_hi)
          )

        if (isTRUE(add_stats)) {
          a <-
            a +
            ggplot2::annotate(
              "text",
              x = 1,
              y = y_hi,
              label = paste(latex2exp::TeX(stats$r[1])),
              vjust = 1,
              hjust = 0,
              parse = TRUE,
              fontface = "bold",
              family = "Helvetica",
              size = 6,
              color = "black"
            )
        } else {
          a = a
        }


      } else if (ymn == 0 & xmn == 0) {
        b = round(stats$b[1], digits = 1)
        m = round(stats$m[1], digits = 1)
        y_lo = b
        y_hi = (m * 7) + b
        xy_ratio = abs(1 / m + b) / 1

        a =
          a +
          ggplot2::scale_y_continuous(
            breaks = seq(round(ymn, digits = 2),
                         round(ymx, digits = 2),
                         length.out = 7),
            expand = c(0.025, 0),
            labels = as.character(formattable::formattable(
              seq(round(ymn, digits = 2),
                  round(ymx, digits = 2),
                  length.out = 7), digits = 2
            ))
          ) +
          ggplot2::scale_x_continuous(
            breaks = seq(round(xmn, digits = 2),
                         round(xmx, digits = 2),
                         length.out = 7),
            expand = c(0.025, 0),
            labels = as.character(formattable(
              seq(round(xmn, digits = 2),
                  round(xmx, digits = 2),
                  length.out = 7), digits = 2
            ))
          ) +
          ggplot2::coord_fixed(
            ratio = 1,
            ylim = c(ymn, ymx),
            xlim = c(xmn, xmx)
          ) +
          ggplot2::theme(strip.background =  element_rect(colour = "white",
                                                          fill = "white"))

        if (isTRUE(add_stats)) {
          a <-
            a +
            ggplot2::annotate(
              "text",
              x = xmn,
              y = ymx,
              label = paste(latex2exp::TeX(stats$r[1])),
              vjust = 1,
              hjust = 0,
              parse = TRUE,
              fontface = "bold",
              family = "Helvetica",
              size = 6,
              color = "black"
            )
        } else{
          a <- a
        }
      }

    }  else if (!is.null(facet_form)) {
      stats_fa <- scatter_stats(statdf <- df,
                                statx <- x,
                                staty <- y,
                                grp_by_form <- grp_by)
      xmx <- max(stats_fa$xmx)
      xmn <- min(stats_fa$xmn)
      ymx <- max(stats_fa$ymx)
      ymn <- min(stats_fa$ymn)

      if (ymn > 0 & xmn > 0) {
        stats_fa =
          stats_fa %>%
          dplyr::mutate(x = 1,
                        y = 7)
        a =
          a +
          ggplot2::scale_x_continuous(breaks = seq(1, 7, 1),
                                      expand = c(0.025, 0)) +
          ggplot2::scale_y_continuous(breaks = seq(1, 7, 1),
                                      expand = c(0.025, 0)) +
          ggplot2::coord_fixed(ratio = 1,
                               xlim = c(1, 7),
                               ylim = c(1, 7)) +
          ggplot2::facet_wrap(stats::reformulate(facet_form), scales = "free") +
          ggplot2::theme(strip.background =  element_rect(colour = "white", fill = "white")) +
          ggplot2::geom_text(
            data = stats_fa,
            aes(1.1, 6.7, color = link, label = paste(TeX(r))),
            inherit.aes = FALSE,
            parse = TRUE,
            vjust = 1,
            hjust = 0,
            fontface = "bold",
            family = "Helvetica",
            size = 6,
            color = "black"
          )


      } else if (ymn == 0 & xmn >= 1) {
        b = round(stats_fa$b[1], digits = 1)
        m = round(stats_fa$m[1], digits = 1)
        y_lo = b
        y_hi = (m * 7) + b
        xy_ratio = abs(1 / m + b) / 1

        stats_fa =
          stats_fa %>%
          dplyr::mutate(x = 1,
                        y = y_hi)

        a =
          a +
          ggplot2::scale_x_continuous(breaks = seq(1, 7, 1),
                                      expand = c(0.025, 0)) +
          ggplot2::scale_y_continuous(breaks = round(seq(y_lo, y_hi, length.out = 7), 2),
                                      expand = c(0.025, 0)) +
          ggplot2::coord_fixed(
            ratio = xy_ratio,
            xlim = c(1, 7),
            ylim = c(y_lo, y_hi)
          ) +
          ggplot2::facet_wrap(stats::reformulate(facet_form), scales = "free") +
          ggplot2::theme(strip.background =  element_rect(colour = "white", fill = "white"))   +
          ggplot2::geom_text(
            data = stats_fa,
            aes(
              1.1,
              y_hi,
              color = link,
              label = paste(latex2exp::TeX(r))
            ),
            inherit.aes = FALSE,
            parse = TRUE,
            vjust = 1,
            hjust = 0,
            fontface = "bold",
            family = "Helvetica",
            size = 6,
            color = "black"
          )

      } else if (ymn == 0 & xmn == 0) {
        b = round(stats_fa$b[1], digits = 1)
        m = round(stats_fa$m[1], digits = 1)
        y_lo = b
        y_hi = (m * 7) + b
        xy_ratio = abs(1 / m + b) / 1


        stats_fa <-
          stats_fa %>%
          dplyr::mutate(x = xmn,
                        y = ymx)

        ypos <- seq(round(ymn, digits = 2),
                    round(ymx, digits = 2),
                    length.out = 7)[7]
        a <-
          a +
          ggplot2::scale_y_continuous(
            breaks = seq(round(ymn, digits = 2),
                         round(ymx, digits = 2),
                         length.out = 7),
            expand = c(0.025, 0),
            labels = as.character(formattable::formattable(
              seq(round(ymn, digits = 2),
                  round(ymx, digits = 2),
                  length.out = 7), digits = 2
            ))
          ) +
          ggplot2::scale_x_continuous(
            breaks = seq(round(xmn, digits = 2),
                         round(xmx, digits = 2),
                         length.out = 7),
            expand = c(0.025, 0),
            labels = as.character(formattable::formattable(
              seq(round(xmn, digits = 2),
                  round(xmx, digits = 2),
                  length.out = 7), digits = 2
            ))
          ) +
          ggplot2::coord_fixed(
            ratio = 1,
            ylim = c(ymn, ymx),
            xlim = c(xmn, xmx)
          ) +
          ggplot2::facet_wrap(stats::reformulate(facet_form), scales = "free") +
          ggplot2::theme(strip.background =  element_rect(colour = "white", fill = "white"))   +
          ggplot2::geom_text(
            data = stats_fa,
            aes(xmn, ypos, label = paste(latex2exp::TeX(r))),
            inherit.aes = FALSE,
            parse = TRUE,
            vjust = 1,
            hjust = 0,
            fontface = "bold",
            family = "Helvetica",
            size = 6,
            color = "black"
          )
      }
    }

  }
