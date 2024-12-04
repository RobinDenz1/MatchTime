
## S3 summary method for MatchTD objects
#' @export
summary.match_td <- function(object, standardize=TRUE, ...) {

  requireNamespace("MatchIt")

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

  # compute sample sizes
  n_matched_treat <- sum(object$trace$new_cases)
  n_matched_controls <- sum(object$trace$matched_controls)

  d_samp <- matrix(c(n_matched_controls, n_matched_treat,
                     object$info$n_unmatched, NA), ncol=2, byrow=TRUE)
  colnames(d_samp) <- c("Controls", "Treated")
  rownames(d_samp) <- c("Matched", "Unmatched")

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
      " in time between ", min(object$trace$time), " and ",
      max(object$trace$time), ".\n", sep="")

  return(invisible(list(balance=d_bal_stats,
                        sample_size=d_samp)))
}
