#' Modified version of tidy
#'
#' @param .data an aovlist object
#' @param ... further arguments passed to or from other methods
#' @return a data.frame with the results of an anova
#' @include sweet_dots-function.R
#' @include sweet_stat-function.R
#' @include sweet_p-function.R
#' @include p_stars-function.R
#' @rdname sweet_tidy
#' @export

sweet_tidy <- function(.data, ...) {
  assertthat::validate_that(class(.data)[1] == "aovlist")
  if (names(.data[1]) == "(Intercept)") {
    aov_res <- .data[-1]
    aov_res
  } else{
    aov_res <- .data
    aov_res
  }
  aov_res_id <-
    aov_res %>%
    lapply(FUN = broom::tidy) %>%
    lapply(dplyr::bind_rows) %>%
    plyr::ldply(.fun = dplyr::bind_rows) %>%
    tibble::as_tibble()
  colnames(aov_res_id)[1] <- "id"

  aov_res_id <-
    aov_res_id[, !(names(aov_res_id) %in% "sumsq")]
  aov_res_id$stars <- apply(aov_res_id[, c("p.value")], MARGIN = 1, FUN = p_stars)
  aov_res_id$mse <- sweet_stat(aov_res_id$meansq)
  aov_res_id$f <-   sweet_stat(aov_res_id$statistic)
  aov_res_id$p <-   sweet_p(aov_res_id$p.value)
  aov_res_id<-aov_res_id[,sapply(names(aov_res_id), FUN=function(x){x %in% c("main_number","set_number","set_id","label","group_id","id","term","df","mse","f","p" ,"stars")})]

}

