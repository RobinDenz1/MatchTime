
## S3 summary method for MatchTime objects
#' @export
summary.match_time <- function(object, standardize=TRUE, remove_unmatched=TRUE,
                               n_required=object$info$ratio, ...) {

  requireNamespace("MatchIt", quietly=TRUE)

  # get relevant columns
  covariates <- covariates_from_match_time(object)

  # get model matrix
  form <- stats::as.formula(paste0("~ ", paste0(covariates, collapse=" + ")))
  mod_mat <- stats::model.matrix(form,
    data=get_match_data(object=object, remove_unmatched=remove_unmatched,
                        n_required=n_required)
  )
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
  bal1var <- utils::getFromNamespace("bal1var", "MatchIt")
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

  # put together sample sizes
  d_samp <- matrix(c(object$sizes$n_matched_controls,
                     object$sizes$n_matched_cases,
                     object$sizes$n_matched_controls +
                       object$sizes$n_matched_cases,
                     object$sizes$n_unmatched_controls,
                     object$sizes$n_unmatched_cases,
                     object$sizes$n_unmatched_controls +
                       object$sizes$n_unmatched_cases,
                     object$sizes$n_incl_controls,
                     object$sizes$n_incl_cases,
                     object$sizes$n_incl_all,
                     object$sizes$n_input_controls,
                     object$sizes$n_input_cases,
                     object$sizes$n_input_all), ncol=3, byrow=TRUE)
  colnames(d_samp) <- c("Controls", "Treated", "All")
  rownames(d_samp) <- c("Matched", "Unmatched", "Included", "Supplied")

  # print it all out
  cat("Call:\n")
  print(object$call)
  cat("\n")
  cat("Summary of Balance for Matched Data at Baseline:\n")
  print(d_bal_stats)
  cat("\nSample Sizes:\n")
  print(d_samp)
  cat("\nPoints in Time:\n")
  cat("Matching was performed at ", nrow(object$trace), " unique points",
      " in time between ", as.character(min(object$trace$time)), " and ",
      as.character(max(object$trace$time)), ".\n", sep="")

  return(invisible(list(balance=d_bal_stats,
                        sample_size=d_samp)))
}

## extracts all relevant covariates from a match_time object
covariates_from_match_time <- function(object) {

  not_rel_cols <- c(object$id, ".id_new", ".id_pair", ".treat", ".treat_time",
                    ".next_treat_time", ".fully_matched", ".weights",
                    ".ps_score", ".prog_score",
                    object$info$added_event_times, object$info$added_status)
  covariates <- colnames(object$data)[!colnames(object$data) %in% not_rel_cols]
  return(covariates)
}
