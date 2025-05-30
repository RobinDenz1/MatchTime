
## S3 print method for MatchTime objects
#' @export
print.match_time <- function(x, ...) {
  cat("A match_time object\n")

  if (x$info$method=="brsm") {
    cat(" - method: balanced risk set matching\n")
  } else if (x$info$method=="psm") {
    cat(" - method: time-dependent propensity score matching\n")
  } else if (x$info$method=="pgm") {
    cat(" - method: time-dependent prognostic score matching\n")
  } else if (x$info$method=="dsm") {
    cat(" - method: time-dependent double score matching\n")
  } else if (x$info$method=="greedy") {
    cat(" - method: always selecting all possible controls\n")
  }

  # matching info
  if (x$info$match_method=="none") {
    cat(" - match-method: 1:", x$info$ratio, " (exact) matching only on time\n",
        sep="")
  } else if (x$info$match_method=="fast_exact") {
    cat(" - match-method: 1:", x$info$ratio, " exact matching\n", sep="")
  } else {
    cat(" - match-method: 1:", x$info$ratio, " ",
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
  cat(" - number of obs.: ", x$sizes$n_input_all, " (original), ",
      x$size$n_matched_cases + x$size$n_matched_controls, " (matched)\n",
      sep="")

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
