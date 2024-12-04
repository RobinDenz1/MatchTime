
## S3 method for bal.tab() function of cobalt package
#' @importFrom cobalt bal.tab
#' @export
bal.tab.match_td <- function(x, s.d.denom, ...) {

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
