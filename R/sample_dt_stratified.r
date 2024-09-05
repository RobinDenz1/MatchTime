
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

  if (!all(names(n) %in% names(ldata))) {
    not_in_dt <- names(n)[!names(n) %in% names(ldata)]

    if (if_lt_n=="stop") {
      stop("Cannot sample ", n[not_in_dt], " rows from 'data' that does not",
           " contain any rows of strata: ", not_in_dt)
    } else if (if_lt_n=="warn") {
      warning("Ignoring strata: ", not_in_dt, " because there are no rows with",
              " such strata in 'data'.")
    } else {
      n <- n[!names(n) %in% not_in_dt]
    }
  }

  out <- Map(sample_dt, ldata, n, replace=replace, if_lt_n=if_lt_n) |>
    rbindlist()
  return(out)
}
