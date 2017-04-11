#' Create a data frame of variable combinations for a fully-crossed factorial design
#'
#' This the generic factor_grid function. See Details.
#'
#' \itemize{
#'   \item \code{\link{factor_grid.list}} for a named list
#'   \item \code{\link{df_to_factgrid}} for a dataframe
#' }
#'
#' @param data data that will be converted to a \code{factor.grid} dataframe
#' @param ... further arguments passed to or from other methods
#' @return a dataframe of class "factor.grid" for passing to additional \link{factorial-design functions}
#' @family factorial-design functions
#' @importFrom magrittr %>%
#' @export
#'
factor_grid <- function(data, ...) {
  UseMethod("factor_grid", data)
}
#' A vectorized vesion of the \code{factor_grid} function
#' @family factorial-design functions
#' @rdname factor_grid
#' @export
factor_grid.default <-
  Vectorize(
    FUN = function(data, ...)
      factor_grid(data, ...),
    vectorize.args = c("data"),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

#' Create a factor grid object from a named list of factors
#' @family factorial-design functions
#' @examples
#' # create a factorial grid data frame by passing a named list of factors/levels
#' factor_grid(list("myvar1" = c("A","B","C","D"),
#' "myvar2" = c("long","short")))
#' @include combine_factor_levels-function.R
#' @export
factor_grid.list <-
  function(data, ...) {

    assertthat::validate_that(is.list(data),
    (!(is.data.frame(data))),
    (!(tibble::is.tibble(data))))

        assertthat::validate_that(assertthat::has_attr(data, "names"))

    options(stringsAsFactors = FALSE)
    data.001 <-
      plyr::llply(names(data), function(iv) {
        d001.01 <-
          as.data.frame(data[[iv]]) %>%
          `names<-`(paste(iv))

        m <-
          nrow(d001.01)

        d001.02 <-
          combine_factor_levels(d001.01,
                       #create matrix of levels
                       m = m:1,
                       simplify = FALSE,
                       byrow = TRUE) %>%
          unlist(use.names = FALSE,
                 recursive = FALSE) %>%
          sapply(simplify = FALSE, paste, collapse = ":") %>% # paste strings
          as.data.frame() %>%
          tidyr::gather(left, condition, 1:ncol(.)) %>%
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
      ) %>%
      stats::as.formula(.)

    data.011 <- #run expand grid
      lazyeval::f_eval(data.010)


    #modify column names
    data.011 <-
      data.011 %>%
      `names<-`(paste(names(data)))

    data.020 <-
      data.011 %>%
      as.data.frame(.) %>%
      dplyr::mutate_if(is.factor, "as.character")

    attr(data.020, "class") <- c("data.frame", "factor.grid")
    data.020
  }


#' Create a \code{factor.grid} df from a data set
#' @param data data that will be converted to a \code{factor.grid} dataframe
#' @param ind_vars a character vector naming the columns to extract factor levels from
#' @family factorial-design functions
#' @examples
#'
#' mydata <- data.frame(
#' factorA = rep(c("sugar","control"), each = 2),
#' factorB = rep(c("coffee","tea"), times = 2),
#' score  = runif(n = 4,min = 10,max = 100)
#' )
#'
#' df_to_factgrid(mydata, c("factorA","factorB"),
#' @importFrom magrittr %>%
#' @include select_dots-function.R
#' @include combine_factor_levels-function.R
#' @export
#'
df_to_factgrid <- function(data, ind_vars) {
  assertthat::validate_that(is.data.frame(data) | tibble::is.tibble(data), assertthat::has_attr(data, "names"))
  assertthat::validate_that(is.vector(ind_vars, mode = "character"))

  options(stringsAsFactors = FALSE)

  data.001 <-
    plyr::llply(ind_vars, function(iv, d = data) {
      assertthat::validate_that(iv %in% names(d))

      d001.01 <-
        d %>%
        dplyr::select_(.dots = select_dots(iv)) %>%
        unique()

      m <-
        nrow(d001.01)

      d001.02 <-
        combine_factor_levels(d001.01,
                              #create matrix of levels
                              m = m:1,
                              simplify = FALSE,
                              byrow = TRUE) %>%
        unlist(use.names = FALSE,
               recursive = FALSE) %>%
        sapply(simplify = FALSE,
               paste,
               collapse = ":") %>% # paste strings
        as.data.frame() %>%
        tidyr::gather(left, condition, 1:ncol(.)) %>%
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
        ) %>%
        stats::as.formula(.)

      data.011 <- #run expand grid
        lazyeval::f_eval(data.010)


      #modify column names
      data.011 <-
        data.011 %>%
        `names<-` (paste0(ind_vars))

      data.020 <-
        data.011 %>%
        as.data.frame(.) %>%
        dplyr::mutate_if(is.factor, "as.character")

      attr(data.020, "class") <-
        c("data.frame", "factor.grid")
      data.020
      attr(data.020)$ind_vars(names(data.020))
}



