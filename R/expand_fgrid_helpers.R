#' Helper functions for creating factor_filter_columns
#' @importFrom magrittr %>%
#' @param .data a df.factor.grid, or part of the grid
#' @param .ind_var one ore more independent variables
#' @param ... additional arguments (ignore)
#' @rdname expand_fgrid_helpers
#' @return a data.frame of factorial-based filtering condition or a  list for passing to additional
#' @export
grow_fgrid<-
  function(.ind_var,
           .data,
           ...) {

    fg_dots <-
      pryr::named_dots(...)
    lapply(fg_dots, eval, parent.frame())

    d <-
      .data %>%
      dplyr::select_(.dots = sweet_dots(.ind_var))

    d2 <-
      lapply(d[,1],
             strsplit,
             split = ":") %>%
      unlist(recursive = FALSE)

    lenNA <- c(is.na(d[1]))
    d$len <- # Create "len" column
      lapply(d2,length) %>%
      stringr::str_c()
    d$len[lenNA] <- "0"
    d$lenNA <- lenNA
    # len1 <- apply(d[2],1, FUN = function(x){ isTRUE(x==1)})

    d$form <-  sapply(d[,1],strsplit,":") %>%
      str_c()
    d$v <- d$form
    d$v <- ifelse(d$len == 1,
                  paste0(paste0(.ind_var),' %in% c("',paste0(d$v),'")'),
                  ifelse(is.na(d$v),NA,
                         paste0(paste0(.ind_var)," %in% ",paste0(d$v))))
    d$form <- d$v
    d$sel <- paste0(.ind_var)
    d$sel[lenNA] <- NA
    d$v <-NULL
    d$lenNA <-NULL


    d <-
      d %>%
      `names<-`(c(paste0(.ind_var,c("_nm", "_len","_form.temp","_sel.temp"))))
    d
  }

#' @rdname expand_fgrid_helpers
#' @return drop levels
#' @export
drop_fgrid_levels <-
  function(.data,...)
  {
    fg_dots <-
      pryr::named_dots(...)
    lapply(fg_dots, eval, parent.frame())

    d <-
      .data[grepl(paste0("_len"), names(.data), fixed = TRUE)]

    keep <-
      apply(d,1,function(x) {
      ifelse(all(x %in% gtools::permutations(n = 2,r = ncol(d),v=c("0","1"),set = FALSE,repeats.allowed = TRUE)),FALSE,TRUE)})

    .data<-.data[keep,]
      .data

  }


#' @rdname expand_fgrid_helpers
#' @return a vector of length of .data
#' @export
number_of_levels <-
  function(.data, ...) {

  assertthat::validate_that(
    is.data.frame(.data) |
      tibble::is.tibble(.data),
    assertthat::has_attr(.data, "names")
  )
  fg_dots <-
    pryr::named_dots(...)
  lapply(fg_dots, eval, parent.frame())

  select_col <-
    "_len"

  .data[grepl(paste0(select_col), names(.data), fixed = TRUE)] %>%
    dplyr::mutate_if(is.character, "as.integer") %>%
    apply(1, sum)
}


#' @rdname expand_fgrid_helpers
#' @return a vector of labels representing crossed-factor levels
#' @export
label_levels <-
  function(.data, ...){
    fg_dots <-
      pryr::named_dots(...)
    lapply(fg_dots, eval, parent.frame())

    select_col <-
      "_nm"

    match_pat <-
      "([:]*)([a-z]{0, })[a-z]*([:]*)"

    replace_pat <-
      "\\1\\2\\3"

    gsub_special <-
      function(data, pattern, replacement)  {
        data_out <-
          gsub(x = data,
               pattern = pattern,
               replacement = replacement,
               ignore.case = TRUE)
        data_out
      }

    data_out <-
      .data[grepl(paste0(select_col), names(.data), fixed = TRUE)]
    data_out <-
      dplyr::mutate_all(data_out,
                        gsub_special,
                        pattern=match_pat,
                        replacement=replace_pat)


    data_out$label <-
      apply(data_out,1,
            function(x) {
              str_c(x[!is.na(x)], collapse = " | ")}) %>%
      lapply(function(x){
        x <-
          ifelse(identical(x,character(0)),NA,x)}) %>%
      unlist(recursive=FALSE)

    data_out<-
      stringr::str_c(data_out$label)
    data_out
  }

#' @rdname expand_fgrid_helpers
#' @return a vector identifying levels for filtering a dataframe
#' @export
filter_from_levels <-
  function(.data, ...){
    assertthat::validate_that(
      is.data.frame(.data) |
        tibble::is.tibble(.data),
      assertthat::has_attr(.data, "names")
    )
    fg_dots <-
      pryr::named_dots(...)
    lapply(fg_dots, eval, parent.frame())

    select_col <-
      "_form.temp"

    data_out <-
      .data[grepl(paste0(select_col),
                  names(.data),
                  fixed = TRUE)]

    data_out$filter_form <-
      apply(data_out,1,
            function(x) {
              str_c(x[!is.na(x)], collapse = " & ")}) %>%
      lapply(function(x){
        x <-
          ifelse(identical(x,character(0)),NA,x)}) %>%
      unlist(recursive=FALSE)

    data_out<-
      stringr::str_c(data_out$filter_form)
    data_out

  }
