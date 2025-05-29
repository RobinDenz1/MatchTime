
## given a start-stop dataset with incomplete information (e.g. missing
## time-intervals inbetween, or possible at the start or end) outputs
## a start-stop dataset with full information
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
#' @export
fill_gaps_start_stop <- function(data, id, start="start", stop="stop",
                                 first_time=NULL, last_time=NULL,
                                 missing_indicator=TRUE, ...) {

  . <- .in_data <- .placeholder <- .start <- .stop <- NULL

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_fill_gaps_start_stop(data=data, id=id, start=start, stop=stop,
                                    missing_indicator=missing_indicator)

  # rename columns to avoid possible issued
  orig_start <- start
  orig_stop <- stop
  setnames(data, old=c(start, stop), new=c(".start", ".stop"))

  # indicator whether interval was already present in data
  data[, .in_data := TRUE]

  # get minimum and maximum time per id
  d_min_max <- data[, .(start = min(.start), stop=max(.stop)),
                    by=eval(id)]
  setnames(d_min_max, old=c("start", "stop"), new=c(".start", ".stop"))
  d_min_max[, .placeholder := TRUE]

  # blow it up using merge_td()
  out <- merge_start_stop(data, d_min_max, by=id, start=".start",
                          stop=".stop", all=TRUE, first_time=first_time,
                          last_time=last_time, ...)
  out[, .placeholder := NULL]

  if (missing_indicator) {
    out[is.na(.in_data), .in_data := FALSE]
  } else {
    out[, .in_data := NULL]
  }

  setnames(out, old=c(".start", ".stop"), new=c(orig_start, orig_stop))

  return(out)
}

## check inputs for fill_gaps_start_stop() function
check_inputs_fill_gaps_start_stop <- function(data, id, start, stop,
                                               missing_indicator) {

  stopifnotm(is_single_character(id) & id %in% colnames(data),
     "'id' must be a single character string specifying a column in 'data'.")
  stopifnotm(is_single_character(start) & id %in% colnames(data),
     "'start' must be a single character string specifying a column in 'data'.")
  stopifnotm(is_single_character(stop) & id %in% colnames(data),
     "'stop' must be a single character string specifying a column in 'data'.")
  stopifnotm(is_single_logical(missing_indicator),
             "'missing_indicator' must be either TRUE or FALSE.")
}
