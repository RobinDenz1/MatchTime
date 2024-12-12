
## user-interface with formula for fast exact matching
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table .SD
#' @importFrom data.table .GRP
#' @export
fast_exact_matching <- function(formula, data, replace=FALSE, ratio=1,
                                estimand="ATT", remove_unmatched=TRUE,
                                n_required=ratio, if_no_match="warn") {
  .strata <- .id_pair <- NULL

  # coerce to data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
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
  data[, .strata := do.call(paste0, .SD), .SDcols=strata_vars]

  # check inputs
  check_inputs_fast_exact_matching(data=data, treat=treat,
                                   strata=".strata", replace=replace,
                                   ratio=ratio, estimand=estimand,
                                   if_no_match=if_no_match)

  # call real matching function
  out <- fast_exact_matching.fit(data=data, treat=treat, strata=".strata",
                                 replace=replace, ratio=ratio,
                                 estimand=estimand, if_no_match=if_no_match)
  out[, .strata := NULL]

  # clean up .id_pair
  out[, .id_pair := .GRP, by=".id_pair"]

  # remove pairs that did not receive n_required matches
  if (remove_unmatched) {
    out <- remove_unmatched(data=out, n_required=n_required)
  }

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

  .id_pair <- .temp_id <- N <- .treat <- .strata <- .weights <- NULL

  # renaming columns to avoid get() issues
  setnames(data, old=c(treat, strata), new=c(".treat", ".strata"))

  # fix treatment variable if needed
  if (!is.logical(data$.treat)) {
    data[, .treat := preprocess_treat(.treat)]
  }

  if (estimand=="ATC") {
    data[, .treat := !.treat]
  }

  # split into cases and controls
  d_controls <- data[.treat==FALSE]
  d_cases <- data[.treat==TRUE]

  # count number of cases per strata
  d_count <- d_cases[, .N, by=".strata"]

  # use stratified sampling to get the right amount of controls for
  # each strata
  size <- d_count$N
  names(size) <- d_count[[".strata"]]

  d_samp <- stratified_sample(data=d_controls,
                              n=size * ratio,
                              strata=".strata",
                              replace=replace,
                              if_lt_n=if_no_match)

  # add pair id for cases
  d_cases[, .id_pair := paste0(.strata, "_", seq_len(.N)), by=".strata"]

  # edge case where no controls could be found
  if (nrow(d_samp)==0) {

    if (estimand=="ATC") {
      d_cases[, .treat := !.treat]
    }

    d_cases[, .weights := 1]
    setnames(d_cases, old=c(".treat", ".strata"), new=c(treat, strata))
    return(d_cases)
  }

  # add pair id for controls
  if (ratio==1) {
    d_samp[, .id_pair := paste0(.strata, "_", seq_len(.N)), by=".strata"]
  } else {
    d_samp <- merge.data.table(d_samp, d_count, by=".strata", all.x=TRUE)
    d_samp[, .temp_id := ceiling(seq_len(.N) / N), by=".strata"]
    d_samp[, .id_pair := paste0(.strata, "_", seq_len(.N)),
           by=c(".strata", ".temp_id")]
    d_samp[, .temp_id := NULL]
    d_samp[, N := NULL]
  }

  # put together
  out <- rbind(d_cases, d_samp)

  if (estimand=="ATC") {
    out[, .treat := !.treat]
  }

  # add matching weights
  set_match_weights(out, treat=".treat", estimand=estimand, keep_ps=FALSE)

  # set names back to original ones
  setnames(out, old=c(".treat", ".strata"), new=c(treat, strata))

  return(out)
}
