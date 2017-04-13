#' Create AOV formulas from a filter.grid
#'
#' Creates a dataframe of formulas for aovs from a filter grid
#'
#' @param data a \code{filter.grid} dataframe
#' @param ... further arguments passed to or from other methods
#' @return a data.frame with a \code{filter.grid} class attribute for passing to additional \link{factorial-design functions}
#' @family factorial-design functions
#' @rdname aov_forms
#' @export
#'
aov_forms <- function(data, ...) {
  UseMethod("aov_forms", data)
}

#' A vectorized version of \code{aov_forms}
#'
#' @param dep_var a vector naming the depend. var.
#' @param ind_var_btwn an optional vector naming the between-subjects variable
#' @family factorial-design functions
#' @rdname aov_forms
#' @export
#'
#'

aov_forms.default <-
  Vectorize(
    FUN = function(data, dep_var, ind_var_btwn, grp_factor)
      aov_forms(data, dep_var, ind_var_btwn, grp_factor),
    vectorize.args = c("data", "dep_var", "grp_factor"),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )


#' Create ANOVA formulas from a filter.grid data frame
#'
#' @include formula_column-functions.R
#' @include select_dots-function.R
#' @family factorial-design functions
#' @rdname aov_forms
#' @export

aov_forms.data.frame <-
  function(data, dep_var, ind_var_btwn, grp_factor) {
    options(stringsAsFactors = FALSE)

    assertthat::validate_that(
      is.data.frame(data) |
        tibble::is.tibble(data),
      assertthat::has_attr(data, "names")
    )


    use_btwn_var <-  # Is there a between-subject IV?
      ifelse(missing(ind_var_btwn), FALSE, TRUE)

    # Prepare between-IV columns for ANOVA formula ----------------------------
    if (isTRUE(use_btwn_var)) {
      data$iv_between <-
        iv_between_col(data, btw_var = ind_var_btwn)
      data$aov_vars_form <-
        aov_vars_col(data)
      data$aov_error_denom <-
        aov_vars_col(data, ind_var_btwn)

      data
    } else {
      data$aov_vars_form <-
        aov_vars_col(data)
      data$aov_error_denom <-
        aov_vars_col(av_data = data)
      data
    }

    # Subset data to create terms for AOV

    aov_create_form_dat <-
      if (isTRUE(use_btwn_var)) {
        data  %>%
          dplyr::bind_rows() %>%
          dplyr::select_(.dots = select_dots(item1 = sapply(
            X = c("iv_between",
                  "aov_vars_form",
                  "aov_error_denom"),
            USE.NAMES = FALSE,
            paste,
            collapse = ","
          )))

      } else {
        data  %>%
          dplyr::bind_rows() %>%
          dplyr::select_(.dots =
                           select_dots(item1 = sapply(
                             X = c("aov_vars_form",
                                   "aov_error_denom"),
                             USE.NAMES = FALSE,
                             paste,
                             collapse = ","
                           )))
      }

    # Create AOV formula
    data$aov_formula <-
      aov_create_form_dat %>%
      plyr::adply(
        .margins = 1,
        .fun =
          function(form_row,
                   dep_var_ = dep_var,
                   err_btwn = use_btwn_var,
                   grp_ = grp_factor)
          {
            # Create Error term
            error_f <-
              if (isTRUE(err_btwn)) {
                ifelse(
                  form_row$aov_vars_form == form_row$iv_between,
                  paste0("Error(", grp_, ") + ", form_row$iv_between),
                  ifelse(
                    form_row$iv_between == "",
                    paste0(
                      "Error(",
                      paste(grp_,
                            paste0("(",
                                   form_row$aov_error_denom, ")"),
                            sep = "/"),
                      ")",
                      form_row$iv_between
                    ),
                    paste0(
                      "Error(",
                      paste(grp_,
                            paste0("(",
                                   form_row$aov_error_denom,
                                   ")"),
                            sep = "/"),
                      ") + ",
                      form_row$iv_between
                    )
                  )
                )
              } else {
                paste0("Error(",
                       paste(grp_,
                             paste0("(", form_row$aov_error_denom, ")"),
                             sep = "/"),
                       ")")
              }
            aov_f_out <-
              paste(paste(dep_var_,
                          form_row$aov_vars_form,
                          sep = " ~ "),
                    error_f,
                    sep = " + ")
            aov_f_out[[1]]
          },
        .expand = FALSE,
        .id = NULL
      ) %>%
      unlist(recursive = FALSE,

             use.names = FALSE)

    data$select_formula <-
      aov_select_col(
        select_data = data,
        s_dep_var = dep_var,
        s_grp_factor = grp_factor
      )
    # Create index xolumn -----------------------------------------------------
    data <-  tibble::rownames_to_column(df = data,
                                        var = "set_number")
    data <-
      data %>%
      dplyr::mutate(
        group_factor = paste0(grp_factor),
        aid1 = toupper(group_factor),
        aid2 = toupper(paste0(dep_var))
      ) %>%
      tidyr::unite(set_id,
                   aid1,
                   aid2,
                   sep = "_",
                   remove = TRUE)


    data <- #drops len (unneeded)
      data %>%
      dplyr::select(-dplyr::contains(match = "_len")) %>%
      dplyr::select(-dplyr::contains(match = "_nm")) %>%
      dplyr::select(-dplyr::contains(match = "_form.temp")) %>%
      dplyr::select(-dplyr::contains(match = "_vars_form")) %>%
      dplyr::select(-dplyr::contains(match = "_denom")) %>%
      dplyr::select(-dplyr::contains(match = "index")) %>%
      dplyr::select(-dplyr::contains(match = "sums")) %>%
      dplyr::select(set_number, label, dplyr::everything()) %>%
      dplyr::mutate(dep_var = paste0(dep_var))

    data <-
      if (isTRUE(use_btwn_var)) {
        data %>%
          dplyr::select(-dplyr::contains(match = "_between"))
      } else{
        data
      }
    data
  }

#' Alternate version of \code{aov_forms}

#' @family factorial-design functions
#' @rdname aov_forms
#' @export
#'
aov_formply <-
   function(data,
            dep_var,
            grp_factor,
            ...) {

 dots <- list(...)

 if(length(dots) == 0){
     out <-
       aov_form.default(
         data = data,
         dep_var = dep_var,
         grp_factor = grp_factor
       )
     out
 }else{

   ind_var_btwn = paste0(dots)
   out<-
   aov_forms.default(
       data = data,
       dep_var = dep_var,
       ind_var_btwn = ind_var_btwn,
       grp_factor = grp_factor
     )
   out
 }
     out <-
       out %>%
       plyr::ldply(bind_rows, .id = "dep_var") %>%
       dplyr::mutate_if(is.factor, "as.character")
     out$main_number <-
       as.character(as.numeric(factor(out$dep_var,
                                      levels = unique(out$dep_var))))
     out<-
       out %>%
       dplyr::select(main_number, set_number, set_id, dplyr::everything()) %>%
       dplyr::mutate_at(c("main_number", "set_number"), "as.numeric")
     out
   }





