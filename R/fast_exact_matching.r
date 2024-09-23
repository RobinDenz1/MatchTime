
## given a data.table containing potential controls and cases,
## return a data.table with all cases and one control with the same strata
## value per case (ratio:1 exact matching on strata)
#' @importFrom data.table .N
#' @importFrom data.table :=
#' @export
fast_exact_matching <- function(data, treat, strata, replace=FALSE,
                                ratio=1, estimand="ATT", if_lt_n="stop") {

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  if (estimand=="ATC") {
    data[, eval(parse(text=treat)) := !eval(parse(text=treat))]
  }

  # split into cases and controls
  d_controls <- data[eval(parse(text=treat))==FALSE]
  d_cases <- data[eval(parse(text=treat))==TRUE]

  # count number of cases per strata
  d_count <- d_cases[, .N, by=strata]

  # use stratified sampling to get the right amount of controls for
  # each strata
  size <- d_count$N
  names(size) <- d_count[[strata]]

  d_samp <- stratified_sample(data=d_controls,
                              n=size * ratio,
                              strata=strata,
                              replace=replace,
                              if_lt_n=if_lt_n)

  # add pair id for cases
  d_cases[, pair_id := paste0(eval(parse(text=strata)), "_", seq_len(.N)),
          by=strata]

  # add pair id for controls
  if (ratio==1) {
    d_samp[, pair_id := paste0(eval(parse(text=strata)), "_", seq_len(.N)),
           by=strata]
  } else {
    d_samp <- merge(d_samp, d_count, by=strata, all.x=TRUE)
    d_samp[, temp_id := ceiling(seq_len(.N) / N), by=strata]
    d_samp[, pair_id := paste0(eval(parse(text=strata)), "_", seq_len(.N)),
           by=c(strata, "temp_id")]
    d_samp[, temp_id := NULL]
    d_samp[, N := NULL]
  }

  # put together
  out <- rbind(d_cases, d_samp)
  return(out)
}
