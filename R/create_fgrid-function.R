#' Create a data frame of variable combinations for a fully-crossed factorial design
#'
#' This the generic create_fgrid function.
#'
#' @param .data data that will be converted to a factorgrid dataframe
#' @param ... additional arguments
#' @return a dataframe of class "factor.grid" for passing to additional factorial-design functions
#' @family factorial-design functions
#' @importFrom magrittr %>%
#' @include sweet_dots-function.R
#' @include combine_factor_levels-function.R

#' @export
#'
create_fgrid <- function(.data, ...) {
  UseMethod("create_fgrid", .data)
}

#' A vectorized vesion of the \code{create_fgrid} function
#' @rdname create_fgrid
#' @export
#'
create_fgrid.default <-
  Vectorize(
    FUN = function(.data, ...)
      create_fgrid(.data, ...),
    vectorize.args = c(".data"),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

#' Create a factor grid object from a named list of factors
#'
#' @rdname create_fgrid
#' @export
create_fgrid.list <-
  function(.data, ...) {
    assertthat::validate_that(is.list(.data),
                              (!(is.data.frame(.data))),
                              (!(tibble::is.tibble(.data))))

    assertthat::validate_that(assertthat::has_attr(.data, "names"))

    options(stringsAsFactors = FALSE)
    name <- names(.data)
    data<-
     suppressWarnings(sapply(names(.data), function(iv) {
        d001.01 <-
          as.data.frame(c(.data[[iv]],NA)) %>%
          `names<-`(paste(iv))

        m <-
          nrow(d001.01)

        d001.01 <-
          combine_factor_levels(d001.01,
                                #create matrix of levels
                                m = m:1,
                                simplify = FALSE,
                                byrow = TRUE) %>%
          unlist(use.names = FALSE,
                 recursive = FALSE)
        d001.01 <-
        t(sapply(d001.01, '[', seq(max(sapply(d001.01, length))))) %>%
          as.data.frame()
        d001.01$set <-apply(d001.01,1, function(x) stringr::str_c(x[!is.na(x)], collapse = ":"))

        d001.01 <- d001.01["set"]
      }))
        data<-
      lapply(data, lapply, function(f) {
        f <- ifelse(identical(f,character(0)),NA,f)
        f}) %>%
        lapply(function(x) as.data.frame(unique(unlist(x))))

          dat_t <- lapply(data,t)
          dat_t <- lapply(dat_t, as.data.frame)
          out <- data.frame(t(plyr::rbind.fill(dat_t)))
          out <-
            out %>%
          magrittr::set_names(value=name)
          data <-
      expand.grid(as.list(out)) %>%
            unique() %>%
      dplyr::mutate_each("as.character")


    attr(data, "class") <-
      c("data.frame", "factor.grid")
    data
  }

#' Create a \code{factor.grid} df from a dataframe
#' @param .ind_vars a character vector naming the columns to extract factor levels from
#' @rdname create_fgrid
#' @export
#'
create_fgrid.data.frame <- function(.data, ..., .ind_vars) {
  assertthat::validate_that(
    is.data.frame(.data) |
      tibble::is.tibble(.data),
    assertthat::has_attr(.data, "names")
  )
  assertthat::validate_that(is.vector(.ind_vars, mode = "character"))

  options(stringsAsFactors = FALSE)

  data.001 <-
    plyr::llply(.ind_vars, function(iv, d = .data) {
      assertthat::validate_that(iv %in% names(d))

      d001.01 <-
        d %>%
        dplyr::select_(.dots = sweet_dots(iv)) %>%
        unique()

      m <-
        nrow(d001.01)

      d001.02 <-
        combine_factor_levels(d001.01,
                              m = m:1,
                              simplify = FALSE,
                              byrow = TRUE) %>%
        unlist(use.names = FALSE,
               recursive = FALSE) %>%
        sapply(simplify = FALSE,
               paste,
               collapse = ":")

      d001.02 <-
        as.data.frame(d001.02)

      number_of_cols <-
        ncol(d001.02)

      left <- NULL
      condition <- NULL
      d001.02 <-
        tidyr::gather(d001.02,
                      key = left,
                      value = condition,
                      1:number_of_cols) %>%
        dplyr::select(condition)
    })
  data.010 <-
    paste0(
      "~reshape::expand.grid.df(",
      paste(
        "as.data.frame(data.001[",
        1:length(data.001),
        "])",
        collapse = ",",
        sep = ""
      ),
      ")"
    )
  data.010 <-
    stats::as.formula(data.010)

  data.011 <- #run expand grid
    lazyeval::f_eval(data.010)


  #modify column names
  data.011 <-
    data.011 %>%
    `names<-` (paste0(.ind_vars))

  data.020 <-
    as.data.frame(data.011) %>%
    dplyr::mutate_if(is.factor, "as.character")

  attr(data.020, "class") <-
    c("data.frame", "factor.grid")
  data.020
  attr(data.020)$ind_vars(names(data.020))
}

