
## given a start-stop dataset with incomplete information (e.g. missing
## time-intervals inbetween, or possible at the start or end) outputs
## a start-stop dataset with full information
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
#' @export
add_missing_intervals <- function(data, id, start="start", stop="stop",
                                  first_time=NULL, last_time=NULL,
                                  copy_data=TRUE, ...) {

  . <- .in_data <- .placeholder <- NULL

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  if (copy_data) {
    data <- copy(data)
  }

  # rename columns to avoid possible issued
  orig_start <- start
  orig_stop <- stop
  setnames(data, old=c(start, stop), new=c("..start..", "..stop.."))
  start <- "..start.."
  stop <- "..stop.."

  # indicator whether interval was already present in data
  data[, .in_data := TRUE]

  # get minimum and maximum time per id
  d_min_max <- data[, .(start = min(get(start)), stop=max(get(stop))),
                    by=eval(id)]
  setnames(d_min_max, old=c("start", "stop"), new=c(start, stop))
  d_min_max[, .placeholder := TRUE]

  # blow it up using merge_td()
  out <- merge_td(data, d_min_max, by=id, start=start, stop=stop, all=TRUE,
                  first_time=first_time, last_time=last_time, ...)
  out[, .placeholder := NULL]
  out[is.na(.in_data), .in_data := FALSE]

  setnames(out, old=c("..start..", "..stop.."), new=c(orig_start, orig_stop))

  return(out)
}
