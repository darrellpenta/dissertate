# source("~/Desktop/apar.R")
slist <-
  l_ply(list.files("~/Desktop/dissertation_functions/"), function(x) {
    source(paste0("~/Desktop/dissertation_functions/", x))
  })


# ---------------------------
# FACTORIAL GRID
# factor_grid (.data, iv)  For factorial design, all unique combinations of indep. vars
#   .data = a df with a colum of each factor;
#   iv=the names of the indep. vars
# generates a data frame of the combinations grouping for the ANOVA
# source: factor_grid-function.R
# ---
# calls: `combn_levels()`, a vectorized version of utils::combine
# source: combn_levels-function.R
# ---------------------------

# ---------------------------
# FACTORIAL FILTER
# factor_filter (.data) Create labels, formulas to filter data, and terms for ANOVA
#   .data = a df with a colum of each factor;
# generates a data frame with rows for filtering anova data
# source: factor_filter-function.R
# ---
# Calls: `select_f()`, creates a dplyr::select_ formula from a character vector
# ---------------------------

# ---------------------------
# ANOVA FORMULA
# aov_formply (source_df, ivf, dv, grp_factor) Build an ANOVA formula
#   source_df = a factor filter df
#   ivf = the name of the column identifying the column with the IV term for the anova
#   dv  = the column name of the DV, or a list of DVs
#   grp_fact = the name of the grouping factor
#
# generates a data frame with formula details for computing ANOVA
# source: aov_formply.R
# ---------------------------

# ---------------------------
# ANOVA OUTPUT
# aov_outply (specs_df, source_df, iv, grp_factor) Run ANOVA and create a tidy summary table
#   specs_df = data frame of specs for ANOVAs
#   source_df = data to be analyzed
#   iv = the names of the IVs
#   grp_fact = the name of the grouping factor
# Computes ANOVA and generate summary
# source: aov_outply.R
# ---
# Calls:`re_tidy()`-- modified version of `broom:tidy()`  for ANOVA with Error term
# source: re_tidy-function.R
# --Calls: `p_star()`-- adds significance starts to summary
# ---------------------------

# ---------------------------
# STATISTIC SWEETENER
# aov_sweeten (.data) Create APA-friendly stats from an aov_outply summary
#   .data = ANOVA summary from aov_outply
# source: aov_sweeten.R
# ---
# Calls `sweetstat()` for non p.value formatting
# source: sweetstat-function.R
# Calls `sweetp()` for p.value
# source: sweetp-function.R
# ---------------------------

# ---------------------------
# ANOVA TO LATEX
# aov_to_latex (.data) Generate LaTeX formatted output of ANOVA stats
# aov_to_latex-function.R
# ---------------------------


# LOAD DATA FRAMES --------------------------------------------------------
# eg. DISCO F1
f1 <-
  readr::read_csv("~/Desktop/Manuscripts/D-ssertation/DISCO/DISCO1/data/disco_f1.csv") %>%
  dplyr::mutate(f = "F1",
                dat.lab = "SUB") %>%
  dplyr::rename(subject = subj) %>%
  dplyr::filter(item.subset == "All")
f2 <-
  readr::read_csv("~/Desktop/Manuscripts/D-ssertation/DISCO/DISCO1/data/disco_f2.csv") %>%
  dplyr::mutate(f = "F2",
                dat.lab = "ITEM") %>%
  dplyr::filter(item.subset == "All") %>%
  dplyr::mutate(item = ifelse(cntbal == "Before", item, item + 100))


# Set list of DVs,IVs, ----------------------------------------------------
# Create factorial grids for analyses

disco <-
  aov_formply(
    factor_grid(data = f1,
                ind_vars =
                  list(cntbal = c(
                    "cntbal", "assoc", "n2num"
                  ))) %>%
      factor_filter() %>%
      .[1],
    dep_var = rep(c("error", "pctND", "asn", "asnND"), each = 2),
    grp_factor = c("subject", "item"),
    "cntbal"
  ) %>%
  dplyr::group_by(main_number, set_number, set_id, label, group_factor)  %>%
  dplyr::do(aov_outply.default(
    spec_row = .,
    src_df = list("subject" = f1, "item" = f2)
  )) %>%
  aov_sweeten() %>%
  aov_fstats_table() %>%
  aov_combine_fstats_long() %>%
  aov_fstats_long_to_wide()




s <-
  readr::read_csv("~/Desktop/Manuscripts/proposal/index/data/0300_exp3_syn_means.csv") %>%
  dplyr::select(diss.num, anim = related, n2num, synonymy, rsyn) %>%
  dplyr::rename(item = diss.num) %>%
  mutate(
    relcond = anim,
    related = case_when(
      .$anim == "AA" | .$anim == "II" ~ "match",
      .$anim == "AI" |
        .$anim == "IA" ~ "mismatch"
    ),
    anim = ifelse(relcond == "AA", "anim", ifelse(relcond == "AI", "anim", "inan"))
  ) %>%
  mutate_at(c("item", "anim", "related", "n2num"), "as.character")




synon <-
  factor_grid.default(data = s, ind_vars = c("anim", "related", "n2num")) %>%
  factor_filter.default() %>%
  aov_formply(dep_var = "synonymy",grp_factor="item") %>%
  dplyr::group_by(set_number, set_id, label, group_factor)  %>%
  dplyr::do(aov_outply(
    spec_row = .,
    src_df = s
  )) %>%
  aov_sweeten() %>%
  aov_fstats_table() %>%
  aov_combine_fstats_long() %>%
  aov_fstats_long_to_wide()


fg <-
  factor_grid(list(cntbal=c("Before","After"), assoc=c("HN","LN","Neutral"), n2num=c("Plural","Singular")))

fg.f <-
  factor_filter(fg)



fg <-
  factor_grid(list(assoc = c("HN","LN","Neutral"),n2num = c("Singular","Plural")))

sel_ <-
  select_dots(c("assoc"))


data <- f1
dv <- "error"
iv <- c("assoc","n2num")
grp_factor<-"subject"
factor_summary <- function(data,
                           dv,
                           iv,
                           grp_factor) {
  assertthat::validate_that(is.character(iv))
  table_len = length(iv) + 4
  dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble("data" = grp_factor),
      data %>%
        ungroup(data) %>%
        dplyr::summarise_at(
          .cols = paste0(dv),
          .funs = dplyr::funs(
            N = length,
            M = mean,
            SD = sd,
            SE = standard_error
          )
        )
    ),
    combine_factor_levels(
      tibble::tibble("data" = iv),
      m = length(iv):1,
      simplify = FALSE,
      byrow = TRUE
    ) %>%
      unlist(recursive = FALSE,
             use.names = FALSE) %>%
      lapply(function(iv_ ,
                      dat = data,
                      table_len_ = table_len,
                      dv_ = dv) {
        dat %>%
          ungroup() %>%
          dplyr::group_by_(.dots = select_dots(iv_)) %>%
          dplyr::summarise_at(
            .cols = paste0(dv_),
            .funs =
              dplyr::funs(
                N = length,
                M = mean,
                SD = sd,
                SE = standard_error
              )
          ) %>%
          as.data.frame(ungroup(.),
                        stringsAsFactors = FALSE)
      }) %>%
      plyr::ldply(function(x,
                           t_len = table_len_) {
        if (ncol(x) != 1) {
          x <-
            x %>%
            tidyr::unite(col = data,
                         -N,
                         -M,
                         -SD,
                         -SE,
                         sep = ":",
                         remove = TRUE)
        } else {
          x <-
            x %>%
            tidyr::unite(col = data,
                         -N,
                         -M,
                         -SD,
                         -SE,
                         sep = "",
                         remove = TRUE)
        }
      })
  ) %>%
    dplyr::arrange(dplyr::desc(N))
}



factor_summary(data = f1, iv = "error", dv = c("assoc","n2num"),"subject")








data__ <- f1 %>% summarize_at(.cols = c(paste0(variable_)), .funs = funs(N=standard_error))


summary_stats(.data = f1,.cols = c("assoc","n2num"), .variable = "error") ->g

c("assoc","n2num") %in% colnames(f1)

data.001 <-
  plyr::llply(c("assoc","n2num"), function(ivs, d = f1) {
    assertthat::validate_that(ivs %in% names(d))})



# utils::update.packages(ask = FALSE, dependencies = c('Suggests'), repos='https://cloud.r-project.org/')
library(stats)
library(lazyeval)
library(plyr)
library(dplyr)
library(devtools)
library(formattable)
library(magrittr)
library(tidyr)
library(stringr)
library(tibble)
library(reshape)
library(reshape2)
library(R.utils)
# install_github("Katiedaisey/ggplot2")
library(ggplot2)
library(broom)
library(broom)
library(effects)
library(knitr)
library(rmarkdown)
library(bookdown)
library(thesisdown)
#devtools::install_github("darrellpenta/dissertate", force = TRUE)
# library(dissertate)
# library(APAstyler)


# install.packages("tidyverse")
# library(tidyverse)


lapply(c("plyr", "tidyr","dplyr","stats","lazyeval","tibble","utils"), devtools::use_package)
# source("~/Desktop/apar.R")
slist <-
  l_ply(list.files("~/Desktop/dissertation_functions/"), function(x) {
    source(paste0("~/Desktop/dissertation_functions/", x))
  })


# ---------------------------
# FACTORIAL GRID
# factor_grid (.data, iv)  For factorial design, all unique combinations of indep. vars
#   .data = a df with a colum of each factor;
#   iv=the names of the indep. vars
# generates a data frame of the combinations grouping for the ANOVA
# source: factor_grid-function.R
# ---
# calls: `combn_levels()`, a vectorized version of utils::combine
# source: combn_levels-function.R
# ---------------------------

# ---------------------------
# FACTORIAL FILTER
# factor_filter (.data) Create labels, formulas to filter data, and terms for ANOVA
#   .data = a df with a colum of each factor;
# generates a data frame with rows for filtering anova data
# source: factor_filter-function.R
# ---
# Calls: `select_f()`, creates a dplyr::select_ formula from a character vector
# ---------------------------

# ---------------------------
# ANOVA FORMULA
# aov_formply (source_df, ivf, dv, grp_factor) Build an ANOVA formula
#   source_df = a factor filter df
#   ivf = the name of the column identifying the column with the IV term for the anova
#   dv  = the column name of the DV, or a list of DVs
#   grp_fact = the name of the grouping factor
#
# generates a data frame with formula details for computing ANOVA
# source: aov_formply.R
# ---------------------------

# ---------------------------
# ANOVA OUTPUT
# aov_outply (specs_df, source_df, iv, grp_factor) Run ANOVA and create a tidy summary table
#   specs_df = data frame of specs for ANOVAs
#   source_df = data to be analyzed
#   iv = the names of the IVs
#   grp_fact = the name of the grouping factor
# Computes ANOVA and generate summary
# source: aov_outply.R
# ---
# Calls:`re_tidy()`-- modified version of `broom:tidy()`  for ANOVA with Error term
# source: re_tidy-function.R
# --Calls: `p_star()`-- adds significance starts to summary
# ---------------------------

# ---------------------------
# STATISTIC SWEETENER
# aov_sweeten (.data) Create APA-friendly stats from an aov_outply summary
#   .data = ANOVA summary from aov_outply
# source: aov_sweeten.R
# ---
# Calls `sweetstat()` for non p.value formatting
# source: sweetstat-function.R
# Calls `sweetp()` for p.value
# source: sweetp-function.R
# ---------------------------

# ---------------------------
# ANOVA TO LATEX
# aov_to_latex (.data) Generate LaTeX formatted output of ANOVA stats
# aov_to_latex-function.R
# ---------------------------


# LOAD DATA FRAMES --------------------------------------------------------
# eg. DISCO F1
f1 <-
  readr::read_csv("~/Desktop/Manuscripts/D-ssertation/DISCO/DISCO1/data/disco_f1.csv") %>%
  dplyr::mutate(f = "F1",
                dat.lab = "SUB") %>%
  dplyr::rename(subject = subj) %>%
  dplyr::filter(item.subset == "All")
f2 <-
  readr::read_csv("~/Desktop/Manuscripts/D-ssertation/DISCO/DISCO1/data/disco_f2.csv") %>%
  dplyr::mutate(f = "F2",
                dat.lab = "ITEM") %>%
  dplyr::filter(item.subset == "All") %>%
  dplyr::mutate(item = ifelse(cntbal == "Before", item, item + 100))


# Set list of DVs,IVs, ----------------------------------------------------
# Create factorial grids for analyses

disco <-
  aov_formply(
    factor_grid(data = f1,
                ind_vars =
                  list(cntbal = c(
                    "cntbal", "assoc", "n2num"
                  ))) %>%
      factor_filter() %>%
      .[1],
    dep_var = rep(c("error", "pctND", "asn", "asnND"), each = 2),
    grp_factor = c("subject", "item"),
    "cntbal"
  ) %>%
  dplyr::group_by(main_number, set_number, set_id, label, group_factor)  %>%
  dplyr::do(aov_outply.default(
    spec_row = .,
    src_df = list("subject" = f1, "item" = f2)
  )) %>%
  aov_sweeten() %>%
  aov_fstats_table() %>%
  aov_combine_fstats_long() %>%
  aov_fstats_long_to_wide()




s <-
  readr::read_csv("~/Desktop/Manuscripts/proposal/index/data/0300_exp3_syn_means.csv") %>%
  dplyr::select(diss.num, anim = related, n2num, synonymy, rsyn) %>%
  dplyr::rename(item = diss.num) %>%
  mutate(
    relcond = anim,
    related = case_when(
      .$anim == "AA" | .$anim == "II" ~ "match",
      .$anim == "AI" |
        .$anim == "IA" ~ "mismatch"
    ),
    anim = ifelse(relcond == "AA", "anim", ifelse(relcond == "AI", "anim", "inan"))
  ) %>%
  mutate_at(c("item", "anim", "related", "n2num"), "as.character")




synon <-
  factor_grid.default(data = s, ind_vars = c("anim", "related", "n2num")) %>%
  factor_filter.default() %>%
  aov_formply(dep_var = "synonymy",grp_factor="item") %>%
  dplyr::group_by(set_number, set_id, label, group_factor)  %>%
  dplyr::do(aov_outply(
    spec_row = .,
    src_df = s
  )) %>%
  aov_sweeten() %>%
  aov_fstats_table() %>%
  aov_combine_fstats_long() %>%
  aov_fstats_long_to_wide()

factorial_grid(list("assoc" = c("HN","LN","Neutral"), "n2num"=c("Singular", "Plural")))
