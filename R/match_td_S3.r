
## S3 print method for MatchTD objects
#' @export
print.MatchTD <- function(x, ...) {
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

## S3 summary method for MatchTD objects
#' @export
summary.MatchTD <- function(object, standardize=TRUE, ...) {

  # get relevant columns
  not_rel_cols <- c(object$id, ".id_new", ".id_pair", ".treat", ".treat_time",
                    ".next_treat_time", object$info$added_events)
  covariates <- colnames(object$data)[!colnames(object$data) %in% not_rel_cols]

  # get model matrix
  form <- stats::as.formula(paste0("~ ", paste0(covariates, collapse=" + ")))
  mod_mat <- stats::model.matrix(form, data=object$data)
  mod_mat <- mod_mat[,seq(2, ncol(mod_mat))]

  # get some defaults for internal functions of MatchIt
  if (standardize) {
    s.d.denom <- switch(object$info$estimand,
                        "ATT" = "treated",
                        "ATC" = "control",
                        "ATE" = "pooled")
  } else {
    s.d.denom <- NULL
  }

  # compute balance statistics
  bal1var <- getFromNamespace("bal1var", "MatchIt")
  bal_stats <- lapply(seq_len(ncol(mod_mat)),
                      function(i) bal1var(mod_mat[,i],
                                          tt=object$data$.treat,
                                          ww=NULL,
                                          s.weights=rep(1, nrow(object$data)),
                                          standardize=standardize,
                                          s.d.denom=s.d.denom,
                                          compute.pair.dist=FALSE))
  d_bal_stats <- do.call("rbind", bal_stats)
  dimnames(d_bal_stats) <- list(colnames(mod_mat), names(bal_stats[[1]]))
  d_bal_stats <- d_bal_stats[, -7]

  # compute sample sizes
  # TODO: finish this function

}

## S3 method for bal.tab() function of cobalt package
#' @importFrom cobalt bal.tab
#' @export
bal.tab.MatchTD <- function(x, s.d.denom, ...) {

  # use only relevant covariates
  not_rel_cols <- c(x$id, ".id_new", ".id_pair", ".treat", ".treat_time",
                    ".next_treat_time", x$info$added_events)
  covariates <- colnames(x$data)[!colnames(x$data) %in% not_rel_cols]

  if (missing(s.d.denom)) {
    s.d.denom <- ifelse(x$info$estimand=="ATT", "treated",
                        ifelse(x$info$estimand=="ATC", "control", "pooled"))
  }

  bal.tab(x=x$data[, covariates, with=FALSE], treat=x$data$.treat,
          s.d.denom=s.d.denom, ...)
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
