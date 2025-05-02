
## simple random samples from a data.table, possibly allowing the resulting
## n to be smaller than the specified one
sample_dt <- function(data, n, replace, if_lt_n, max_replace=NULL) {

  if (if_lt_n=="stop" && nrow(data) < n && !replace) {
    stop("Cannot sample ", n, " rows from a data.table with only ",
         nrow(data), " rows if replace=FALSE.", call.=FALSE)
  } else if (if_lt_n=="warn" && nrow(data) < n && !replace) {
    warning("Could only sample ", nrow(data), " rows from 'data' instead",
            " of ", n, " rows using replace=FALSE.")
  }

  if (!is.null(max_replace) && if_lt_n=="stop" && replace &&
      (nrow(data) * max_replace) < n) {
    stop("Cannot sample ", n, " rows from a data.table with only ",
         nrow(data), " rows if each unit can only be replaced ",
         max_replace, " times.", call.=FALSE)
  } else if (!is.null(max_replace) && if_lt_n=="warn" && replace &&
             (nrow(data) * max_replace) < n) {
    warning("Could only sample ", nrow(data)*max_replace, " rows from 'data'",
            " instead of ", n, " rows using replace=TRUE and max_replace=",
            max_replace, ".")
  }

  if (!replace) {
    n <- min(c(n, nrow(data)))
  } else if (replace & !is.null(max_replace)) {
    n <- min(c(n, nrow(data)*max_replace))
  }

  if (is.null(max_replace)) {
    out <- data[sample.int(n=nrow(data), replace=replace, size=n)]
  } else {
    out <- data[sample(x=rep(seq_len(nrow(data)), max_replace),
                       replace=FALSE, size=n)]
  }

  return(out)
}

## fast and efficient stratified random sampling from a data.table
#' @importFrom fastmatch %fin%
#' @importFrom data.table rbindlist
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @export
stratified_sample <- function(data, n, strata, replace=FALSE,
                              max_replace=NULL, if_lt_n="stop") {

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  ldata <- split(data, by=strata)

  # in case the data does not contain the strata specified by n
  if (!all(names(n) %fin% names(ldata))) {
    not_in_dt <- names(n)[!names(n) %fin% names(ldata)]

    if (if_lt_n=="stop") {
      stop("Cannot sample ", n[not_in_dt], " rows from strata '", not_in_dt,
           "' in 'data' because there are no rows in these strata.",
           call.=FALSE)
    } else if (if_lt_n=="warn") {
      warning("Ignoring strata: ", not_in_dt, " because there are no rows with",
              " such strata in 'data'.")
    }
    n <- n[!names(n) %fin% not_in_dt]
  }

  # sample from each stratum
  out <- vector(mode="list", length=length(n))
  for (i in names(n)) {

    if (!is.null(max_replace)) {
      max_replace_i <- max_replace[[i]]
    } else {
      max_replace_i <- NULL
    }

    out[[i]] <- sample_dt(data=ldata[[i]], n=n[[i]], replace=replace,
                          if_lt_n=if_lt_n, max_replace=max_replace_i)
  }

  return(rbindlist(out))
}
