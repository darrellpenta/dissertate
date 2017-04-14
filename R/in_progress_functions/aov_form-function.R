# # Build ANOVA formula
# # ------------------------------------
# aov_form <- function(dv,
#                      iv,
#                      grp_factor) {
#
#
#
#    iv_f <-
#            paste0(iv,
#                   sep = "",
#                   collapse = "*")
#
#     error_f <-
#       paste0("Error(",
#              paste(grp_factor, paste0("(", iv_f, ")"), sep = "/"),
#              ")")
#
#     aov_form <-
#                paste(paste(dv, iv_f, sep = "~"), error_f, sep = "+") %>%
#       stats::as.formula(.)
#     aov_form
# }
#
