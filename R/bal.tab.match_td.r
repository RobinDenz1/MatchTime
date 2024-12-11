
## S3 method for bal.tab() function of cobalt package
#' @importFrom cobalt bal.tab
#' @export
bal.tab.match_td <- function(x, s.d.denom, remove_unmatched=TRUE,
                             n_required=x$info$ratio, ...) {

  # use only relevant covariates
  not_rel_cols <- c(x$id, ".id_new", ".id_pair", ".treat", ".treat_time",
                    ".next_treat_time", ".fully_matched", ".weights",
                    x$info$added_event_times, x$info$added_status)
  covariates <- colnames(x$data)[!colnames(x$data) %in% not_rel_cols]

  if (missing(s.d.denom)) {
    s.d.denom <- ifelse(x$info$estimand=="ATT", "treated",
                        ifelse(x$info$estimand=="ATC", "control", "pooled"))
  }

  if (remove_unmatched){
    data <- match_data(object=x, remove_unmatched=remove_unmatched,
                       n_required=n_required)
    treat <- data$.treat
    data <- data[, covariates, with=FALSE]
  } else {
    treat <- x$data$.treat
    data <- x$data[, covariates, with=FALSE]
  }

  bal.tab(x=data, treat=treat, s.d.denom=s.d.denom, ...)
}
