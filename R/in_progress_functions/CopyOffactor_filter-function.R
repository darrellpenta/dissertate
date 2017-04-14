#' #' Data-filtering conditions as vectors
#' #'
#' #' @family factorial-design functions
#' #' @rdname factor_filter
#' #' @export
#' factor_filter.default <-
#'   Vectorize(
#'     FUN = function(data, ...)
#'       factor_filter(data, ...),
#'     vectorize.args = c("data"),
#'     SIMPLIFY = FALSE,
#'     USE.NAMES = FALSE
#'   )
#'
#' #' Derive filtering conditions from a factor grid
#' #' @param trunc_label Should the output contain a column with printing/reading-friendly, truncuated labels to indicate which factor(s)/levels(s) are being used to filter? Default is TRUE. Mutlple levels of a single factor are separated with a colon: (e.g., <code>fact1level1:fact1level2</code>); factors are separated by a double-colon: (e.g., <code>fact1level1:fact1level2::fact2level1</code>)
#' #' @param for_aov A logical indicating whether the data be returned for passing to a function in @family aov-out functions? Defaults to TRUE.
#' #' @importFrom magrittr %>%
#' #' @family factorial-design functions
#' #' @examples
#' #'
#' #' my_filtertgrid <- factor_filter(my_factgrid)
#' #' )
#' #' @include select_dots-function.R
#' #' @export
#'
#'
#' factor_filter.factor.grid <-
#'   function(data,
#'            trunc_label = TRUE,
#'            for_aov = TRUE,
#'            ...) {
#'     assertthat::validate_that(
#'       assertthat::has_attr(data, which = "class"),
#'       "factor.grid" %in% attr(data, which = "class")
#'     )
#'     options("stringsAsFactors" = FALSE)
#'     iv <-
#'       c(paste(colnames(data)))
#'
#'
#'     dat.001 <- #loop over each named independent variable
#'       lapply(iv, function(iv.001,
#'                           dat.001.01 = data) {
#'         dat.002 <- #subset data one variable at a time
#'           dat.001.01 %>%
#'           dplyr::select_(.dots = select_dots(iv.001))
#'
#'         dat.003 <- #renaming df allows for flexible expansion
#'           tibble::tibble("col1" = dat.002[[1]])
#'
#'         dat.003.01 <-
#'           lapply(dat.003$col1,
#'                  strsplit,
#'                  split = ":") %>%
#'           unlist(recursive = FALSE)
#'
#'         dat.002$len <-
#'           # Setting the length (of the vector) to keep or drop variable accordingly
#'           lapply(dat.003.01,
#'                  length) %>%
#'           stringr::str_c()
#'
#'         dat.002$form <-
#'           # dynamic creation of filtering conditions for analyses
#'           lapply(dat.003.01,
#'                  function(dat.003.02) {
#'                    dat.003.02 <-
#'                      paste(iv.001,
#'                            paste0(
#'                              "c(",
#'                              paste(
#'                                "'",
#'                                dat.003.02,
#'                                "'",
#'                                collapse = ",",
#'                                sep = ""
#'                              ),
#'                              ")"
#'                            ),
#'                            sep = " %in% ")
#'                  }) %>%
#'           stringr::str_c()
#'
#'         dat.002 <- #rename tiny data set with name of variable
#'           dat.002 %>%
#'           `names<-`(c(
#'             paste0(iv.001, "_nm"),
#'             paste0(iv.001, "_len"),
#'             paste0(iv.001, "_form.temp")
#'           ))
#'       }) %>%
#'       as.data.frame()
#'
#'     dat.001$num_lvls <-
#'       # summing the no. of levels invovled in analyses
#'       dat.001 %>%
#'       dplyr::select(dplyr::contains(match = "_len")) %>%
#'       dplyr::mutate_if(is.character, "as.integer") %>%
#'       apply(1, sum)
#'
#'     dat.001$label <- #labelling to make printing easier
#'       dat.001 %>%
#'       dplyr::select(dplyr::contains(match = "_nm")) %>%
#'       dplyr::mutate_all(function(x) {
#'         gsub(
#'           x = x,
#'           pattern = "([:]*)([a-z]{0,3})[a-z]*([:]*)",
#'           "\\1\\2\\3",
#'           ignore.case = TRUE
#'         )
#'       }) %>%
#'       tidyr::unite(label,
#'                    dplyr::everything(),
#'                    sep = "::") %>%
#'       .$label
#'
#'     dat.001$filter_formula <-
#'       #combines filtering conditions for variables
#'       dat.001 %>%
#'       dplyr::select(dplyr::contains(match = "_form.temp")) %>%
#'       tidyr::unite(filter_f,
#'                    dplyr::contains(match = "_form.temp"),
#'                    sep = " & ") %>%
#'       .$filter_f
#'
#'     if (isTRUE(for_aov) | missing(for_aov)) {
#'       #orders the data from largest analyses to smallest, removes illogical groupings
#'       dat.001 <-
#'         dat.001 %>%
#'         dplyr::filter(!(num_lvls %in% min(dat.001$num_lvls))) %>%
#'         dplyr::arrange(dplyr::desc(num_lvls))
#'       dat.001
#'     }
#'
#'     dat.001 <-
#'       dat.001 %>%
#'       dplyr::select(-dplyr::contains(match = "_len")) %>%
#'       dplyr::select(-dplyr::contains(match = "_nm")) %>%
#'       dplyr::select(-dplyr::contains(match = "_form.temp"))
#'
#'     if (isTRUE(trunc_label) | missing(trunc_label)) {
#'       dat.001 <-
#'         dat.001 %>%
#'         dplyr::select(label, num_lvls, dplyr::everything())
#'       dat.001
#'
#'     } else {
#'       dat.001 <-
#'         dat.001 %>%
#'         dplyr::select(num_lvls, dplyr::everything())
#'     }
#'
#'
#'
#'     attr(dat.001, "class") <- c("data.frame", "filter.grid")
#'
#'     dat.001
#'
#'
#'   }
#'
#' # #
#' #
#' # ----------
#' #     # dat.001 <- #drops len (unneeded)
#' #     #   dat.001 %>%
#' #     #   dplyr::select(-dplyr::contains(match = "_len")) %>%
#' #     #   dplyr::select(-dplyr::contains(match = "_nm")) %>%
#' #     #   dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
#' #     #   dplyr::select(label,num_lvls, dplyr::everything())
#' #
#' #     dat.001
#' #
#' #     # dat.001$iv_f <- #creates the term for the IVs in the anova
#' #     #   ldply(iv, function(z,
#' #     #                      ivar = dat.001) {
#' #     #     ivar_out <-
#' #     #       ivar %>%
#' #     #       dplyr::select_(.dots = select_f(item1 = paste0(z, "_len")))
#' #     #
#' #     #     ivar_out[[1]] <-
#' #     #       ifelse(ivar_out[[1]] == 1,
#' #     #              NA_character_,
#' #     #              paste(z))
#' #     #   }) %>%
#' #     #   apply(MARGIN = 2,
#' #     #         paste,
#' #     #         collapse = ":") %>%
#' #     #   sapply(FUN =  gsub,
#' #     #          pattern = "[:]{1}[NA]{2}|[NA]{2}[:]{1}",
#' #     #          replacement = "") %>%
#' #     #   tibble::as_tibble() %>%
#' #     #   dplyr::mutate(iv_f = gsub(
#' #     #     pattern = ":",
#' #     #     " * ",
#' #     #     fixed = TRUE,
#' #     #     x = value
#' #     #   )) %>%
#' #     #   .$iv_f
#' #     #
#' #
#' #   }
#' #
#' #
#' # factor_filter <-
#' #   Vectorize(
#' #     FUN = function(data)
#' #       factor_filter.default(data),
#' #     vectorize.args = c("data"),
#' #     SIMPLIFY = FALSE,
#' #     USE.NAMES = FALSE
#' #   )
