
## given a data.table containing event times per person and a duration of
## that event, adds an indicator to the matched data which is TRUE if the
## event was currently going on when the person got matched and FALSE otherwise
#' @importFrom data.table :=
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
add_previous_event_time <- function(data, d_prev, id, time, duration,
                                    name, include_same_t=FALSE) {

  .in_risk <- .treat_time <- .prev_time <- .id_new <- .next_treat_time <- NULL

  # merge to matched data, creating new rows
  setnames(d_prev, old=time, new=".prev_time")
  data <- merge.data.table(data, d_prev, by=id, all.x=TRUE)

  # check if .treat_time is in any of the previous risk periods by event
  # NOTE: the way I simulated the data this argument should be TRUE for
  #       time-dependent covariates, but FALSE for outcomes
  if (include_same_t) {
    data[, .in_risk := .treat_time < (.prev_time + eval(duration)) &
           .treat_time >= .prev_time]
  } else {
    data[, .in_risk := .treat_time < (.prev_time + eval(duration)) &
           .treat_time > .prev_time]
  }

  data[is.na(.in_risk), .in_risk := FALSE]
  data[, .in_risk := any(.in_risk), by=.id_new]
  data[, .prev_time := NULL]

  # remove duplicate rows
  data <- unique(data)
  setnames(data, old=".in_risk", new=name)

  return(data)
}

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
