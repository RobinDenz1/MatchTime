
## S3 print method for MatchTD objects
#' @export
print.match_td <- function(x, ...) {
  cat("A MatchTD object\n")

  # matching info
  if (x$info$match_method=="none") {
    cat(" - method: 1:", x$info$ratio, " (exact) matching only on time\n",
        sep="")
  } else if (x$info$match_method=="fast_exact") {
    cat(" - method: 1:", x$info$ratio, " exact matching\n", sep="")
  } else {
    cat(" - method: 1:", x$info$ratio, " ",
        get_matchit_method_str(x$info$match_method),
        " (using matchit())\n", sep="")
  }

  # replacement of controls
  if (x$info$replace_at_t & x$info$replace_over_t) {
    replace_c_str <- "Replacing controls at each point in time and over time\n"
  } else if (x$info$replace_at_t) {
    replace_c_str <- "Replacing controls only at each point in time\n"
  } else if (x$info$replace_over_t) {
    replace_c_str <- "Replacing controls only over time\n"
  } else {
    replace_c_str <- "Not replacing controls\n"
  }

  cat(" - controls: ", replace_c_str, sep="")

  # replacement of cases
  if (x$info$replace_cases) {
    replace_e_str <- "Using all cases\n"
  } else {
    replace_e_str <- "Using only cases that were not used as controls\n"
  }

  cat(" - cases: ", replace_e_str, sep="")

  # number of observations
  cat(" - number of obs.: ", x$info$n_orig, " (original), ",
      x$info$n_matched, " (matched)\n", sep="")

  # target estimand
  cat(" - target estimand: ", x$info$estimand, "\n", sep="")

  # covariates
  if (is.null(x$info$match_vars)) {
    cat(" - covariates: Only matched on time", sep="")
  } else {
    var_str <- paste0(x$info$match_vars, collapse=", ")
    var_str <- substr(var_str, 1, nchar(var_str))
    cat(" - covariates: ", var_str, "\n", sep="")
  }
}

# obtain string describing method in matchit()
get_matchit_method_str <- function(method) {
  switch(method,
         "exact" = "exact matching",
         "cem" = "coarsened exact matching",
         "nearest" = "nearest neighbor matching",
         "optimal" = "optimal pair matching",
         "full" = "optimal full matching",
         "quick" = "generalized full matching",
         "genetic" = "genetic matching",
         "cardinality" = "cardinality matching")
}
