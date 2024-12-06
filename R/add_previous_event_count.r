
## counts all events that happened "duration" time-units before time
#' @importFrom data.table :=
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
add_previous_event_count <- function(data, d_prev, id, id_new, time, duration,
                                     name, include_same_t=FALSE) {

  . <- .prev_time <- .count <- .diff <- NULL

  setnames(d_prev, old=time, new=".prev_time")
  data <- merge.data.table(data, d_prev, by=id, all.x=TRUE)
  data[, .diff := as.vector(.prev_time - get(time))]

  if (include_same_t) {
    out <- data[, .(.count = sum(.diff <= 0 & .diff >= -duration)),
                by=eval(id_new)]
  } else {
    out <- data[, .(.count = sum(.diff < 0 & .diff >= -duration)),
                by=eval(id_new)]
  }
  out[is.na(.count), .count := 0]
  setnames(out, old=".count", new=name)

  return(out)
}
