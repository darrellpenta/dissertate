#' Create AOV fstats_table
#'
#' Creates a table of aov summary
#' @importFrom magrittr %>%
#' @param .data data to be analysed
#' @param stat_ls list of stat designations for printout
#' @param latex shoudl the output be latex?
#' @param interval should pvalues be returned as intervals?
#' @param ... further arguments passed to or from other methods (ignored)
#' @return a data.frame with the results of an anova
#' @rdname aov_fstats_table
#' @export

aov_fstats_combine <- function(.data, stat_ls,  interval, ...) {
  afs_dots <-
    pryr::named_dots(...)
  lapply(afs_dots, eval, parent.frame())

  data_names <- names(.data)

  data <- .data
  data <-
    data %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
    magrittr::set_names(value=data_names)
  bind <-
    paste(data$main_number, data$set_number, data$set_id,.data$label, data$group_id,data$id, sep = "_")
  data$bind <-
    bind
  data <-
    data[,sapply(names(data), FUN=function(x){!x %in% c("main_number","set_number","set_id","label","group_id","id")})]

data_left<-
        dplyr::filter_(data, .dots = sweet_dots("term != 'Residuals'")) %>%
        dplyr::select_(.dots = flist_to_dots("list(~bind,~term,~df, ~f,~p, ~flabel, ~stars)"))
data_right <-
        dplyr::filter_(data, .dots = sweet_dots("term == 'Residuals'")) %>%
        dplyr::select_(.dots = flist_to_dots("list(~bind,~df,~mse, ~stars)"))
names(data_right) <- c("bind","r_df","mse","f2stars")

  data <-
    dplyr::left_join(data_left,data_right,by = "bind")


  statcol <- paste0('~dplyr::mutate(.data=data, stats = paste0(
            flabel,"(",df,",",r_df,")",
            stat_ls["eq"][[1]],f,", ",
            stat_ls["mse"][[1]],mse,", ",
            stat_ls["p"][[1]],
            ifelse(isTRUE(interval),p,ifelse(p == ".001",stat_ls["less"][[1]],stat_ls["eq"][[1]]),p)))')

  statcol <-
    stats::as.formula(statcol)

  data <-
    lazyeval::f_eval(statcol, data = data)

 starscol <- paste0('~dplyr::mutate(.data=data, stars = paste0(ifelse(stars == "NA" | is.na(stars) | stars == "n.s."," ",
            stars)," : ",ifelse(f2stars == "NA" | is.na(f2stars) | f2stars == "n.s."," ",
            f2stars)))')

  starscol <-
    stats::as.formula(starscol)

  data <-
    lazyeval::f_eval(starscol, data = data)



  data<-
    data[c("term","stats","stars")]
 data
  }

#' @rdname aov_fstats_table
#' @export

aov_fstats_table <-
  function(.data,
           latex = TRUE, ...
           ) {
    if (isTRUE(latex)) {
      stat_list <- list(
        "eq" = paste0(" $=$ "),
        "less" = paste0(" < "),
        "f" = paste0("\\ext{F}"),
        "f1" = paste0("\\ext{F$_1$}"),
        "f2" = paste0("\\ext{F$_2$}"),
        "mse" = paste0("\\ext{MS$_e$} $=$ "),
        "p" = paste0("\\ext{p}")
      )
      stat_list
    } else {
      stat_list <- list(
        "eq" = paste0(" = "),
        "less" = paste0(" < "),
        "f" = paste0("F"),
        "f1" = paste0("F1"),
        "f2" = paste0("F2"),
        "mse" = paste0("MSe = "),
        "p" = paste0("p")
      )
      stat_list
    }

data_names <- names(.data)
   .data <-
      .data %>%
      dplyr::ungroup() %>%
      dplyr::mutate_all("as.character") %>%
      magrittr::set_names(value = data_names)


    # if (isTRUE(fs)) {
     flabel <- ifelse(.data$group_id == "subject",stat_list["f1"][[1]],stat_list["f2"][[1]]
          )
.data$flabel <- flabel
    # } else {
    #   out_aov_ <-
    #     out_aov_ %>%
    #     dplyr::mutate(flabel = stat_list["f"][[1]])
    #   out_aov_
    # }




  do_run <- paste0('~dplyr::group_by(.data, main_number,set_number,set_id, label, group_id, id) %>%
                   dplyr::do(aov_fstats_combine(.data = .,stat_ls=stat_list))')
  do_run <-
    stats::as.formula(do_run)

    .data <-
      lazyeval::f_eval(do_run,data = .data)


    .data$main_number  <-as.numeric(.data$main_number)
    .data$set_number  <-as.numeric(.data$set_number)
.data

  }

