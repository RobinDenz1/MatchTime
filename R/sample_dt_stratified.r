
## simple random samples from a data.table, possibly allowing the resulting
## n to be smaller than the specified one
sample_dt <- function(data, n, replace, if_lt_n) {

  if (if_lt_n=="stop" && nrow(data) < n) {
    stop("Cannot sample ", n, " rows from a data.table with only ",
         nrow(data), " rows if replace=FALSE.")
  } else if (if_lt_n=="warn" && nrow(data) < n) {
    warning("Could only sample ", nrow(data), " rows from 'data' instead",
            " of ", n, " rows.")
  }
  n <- min(c(n, nrow(data)))

  return(data[sample.int(n=nrow(data), replace=replace, size=n)])
}

## fast and efficient stratified random sampling from a data.table
#' @importFrom data.table rbindlist
#' @export
sample_dt_stratified <- function(data, n, strata, replace=FALSE,
                                 if_lt_n="stop") {

  ldata <- split(data, by=strata)

  # in case the data does not contain the strata specified by n
  if (!all(names(n) %fin% names(ldata))) {
    not_in_dt <- names(n)[!names(n) %fin% names(ldata)]

    if (if_lt_n=="stop") {
      stop("Cannot sample ", n[not_in_dt], " rows from strata '", not_in_dt,
           "' in 'data' because there are no rows in these strata.")
    } else if (if_lt_n=="warn") {
      warning("Ignoring strata: ", not_in_dt, " because there are no rows with",
              " such strata in 'data'.")
    } else {
      n <- n[!names(n) %fin% not_in_dt]
    }
  }

  # in case the strata specified by n do not include all strata in data
  # add them and set them to 0
  if (!all(names(ldata) %fin% names(n))) {
    not_in_n <- names(ldata)[!names(ldata) %fin% names(n)]
    n[not_in_n] <- 0
  }

  # sample from each stratum
  out <- vector(mode="list", length=length(n))
  for (i in names(n)) {
    out[[i]] <- sample_dt(data=ldata[[i]], n=n[[i]], replace=replace,
                          if_lt_n=if_lt_n)
  }

  return(rbindlist(out))
}
