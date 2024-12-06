
## counts all events that happened "duration" time-units before time
#' @importFrom data.table :=
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
#' @export
add_previous_event_count <- function(x, data, id=x$id, time=x$time, duration,
                                     include_same_t=TRUE, units="auto",
                                     name=".prev_event_count") {

  . <- .time <- .count <- .diff <- .treat_time <- NULL

  x <- copy(x)

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_add_variable(x=x, data=data, id=id, time=time,
                            include_same_t=include_same_t,
                            name=name)

  # make names of columns consistent
  setnames(data, old=time, new=".time")

  if (id!=x$id) {
    setnames(data, old=id, new=x$id)
  }

  # merge to matched data, creating new rows
  data <- merge.data.table(x$data, data, by=x$id, all.x=TRUE, sort=FALSE)

  # calculate difference between event time and inclusion time
  if (all(class(data$.treat_time) %in% c("Date", "POSIXct", "POSIXlt"))) {
    data[, .diff := as.numeric(difftime(.time, .treat_time, units=units))]
  } else {
    data[, .diff := .time - .treat_time]
  }

  # count number of events per .id_new
  if (include_same_t) {
    out <- data[, .(.count = sum(.diff <= 0 & .diff >= -duration)),
                by=".id_new"]
  } else {
    out <- data[, .(.count = sum(.diff < 0 & .diff >= -duration)),
                by=".id_new"]
  }
  out[is.na(.count), .count := 0]

  # merge back to original data
  x$data <- merge.data.table(x$data, out, by=".id_new", all.x=TRUE, sort=FALSE)
  setnames(x$data, old=".count", new=name)
  setkeyv(x$data, c(x$id))

  return(x)
}
