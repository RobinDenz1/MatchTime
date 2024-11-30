
## user-interface with formula for fast exact matching
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table .SD
#' @export
fast_exact_matching <- function(formula, data, replace=FALSE, ratio=1,
                                estimand="ATT", if_no_match="warn",
                                check_inputs=TRUE, copy_data=TRUE) {
  ..strata.. <- NULL

  # coerce to data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else if (copy_data) {
    data <- copy(data)
  }

  # treatment variable
  vars <- all.vars(formula)
  treat <- vars[1]

  # strata variables
  if (length(vars) > 1) {
    strata_vars <- vars[2:length(vars)]
  } else {
    stop("'formula' needs to contain at least one variable on the right-hand",
         " side to match on.")
  }

  # create strata variable
  data[, ..strata.. := do.call(paste0, .SD), .SDcols=strata_vars]

  # check inputs if specified
  if (check_inputs) {
    check_inputs_fast_exact_matching(data=data, treat=treat,
                                     strata="..strata..", replace=replace,
                                     ratio=ratio, estimand=estimand,
                                     if_no_match=if_no_match)
  }

  # call real matching function
  out <- fast_exact_matching.fit(data=data, treat=treat, strata="..strata..",
                                 replace=replace, ratio=ratio,
                                 estimand=estimand, if_no_match=if_no_match)
  out[, ..strata.. := NULL]

  return(out)
}

## given a data.table containing potential controls and cases,
## return a data.table with all cases and one control with the same strata
## value per case (ratio:1 exact matching on strata)
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table merge.data.table
fast_exact_matching.fit <- function(data, treat, strata, replace=FALSE,
                                    ratio=1, estimand="ATT",
                                    if_no_match="stop") {
  pair_id <- temp_id <- N <- ..treat.. <- ..strata.. <- NULL

  # renaming columns to avoid get() issues
  setnames(data, old=c(treat, strata), new=c("..treat..", "..strata.."))

  if (estimand=="ATC") {
    data[, ..treat.. := !..treat..]
  }

  # split into cases and controls
  d_controls <- data[..treat..==FALSE]
  d_cases <- data[..treat..==TRUE]

  # count number of cases per strata
  d_count <- d_cases[, .N, by="..strata.."]

  # use stratified sampling to get the right amount of controls for
  # each strata
  size <- d_count$N
  names(size) <- d_count[["..strata.."]]

  d_samp <- stratified_sample(data=d_controls,
                              n=size * ratio,
                              strata="..strata..",
                              replace=replace,
                              if_lt_n=if_no_match)

  # add pair id for cases
  d_cases[, pair_id := paste0(..strata.., "_", seq_len(.N)), by="..strata.."]

  # edge case where no controls could be found
  if (nrow(d_samp)==0) {
    setnames(d_cases, old=c("..treat..", "..strata.."), new=c(treat, strata))
    return(d_cases)
  }

  # add pair id for controls
  if (ratio==1) {
    d_samp[, pair_id := paste0(..strata.., "_", seq_len(.N)), by="..strata.."]
  } else {
    d_samp <- merge.data.table(d_samp, d_count, by="..strata..", all.x=TRUE)
    d_samp[, temp_id := ceiling(seq_len(.N) / N), by="..strata.."]
    d_samp[, pair_id := paste0(..strata.., "_", seq_len(.N)),
           by=c("..strata..", "temp_id")]
    d_samp[, temp_id := NULL]
    d_samp[, N := NULL]
  }

  # put together
  out <- rbind(d_cases, d_samp)

  # set names back to original ones
  setnames(out, old=c("..treat..", "..strata.."), new=c(treat, strata))

  return(out)
}
