
## given the current matched data and a data.table containing none, one or
## multiple events per person, add the next event after .treat_time
#' @importFrom data.table :=
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
add_next_event_time <- function(data, d_event, id, time, include_same_t=TRUE,
                                next_time_name) {

  .is_after <- .next_event_time <- .treat_time <- .id_new <- .in_risk <- NULL

  # merge to matched data, creating new rows
  setnames(d_event, old=time, new=".next_event_time")
  data <- merge.data.table(data, d_event, by=id, all.x=TRUE,
                           allow.cartesian=TRUE)

  # check if event is after inclusion time
  # NOTE: the way I simulated the data this argument should be TRUE for
  #       outcomes, but FALSE for time-dependent variables
  #       (although it is unclear why it would be used for the latter)
  if (include_same_t) {
    data[, .is_after := .next_event_time >= .treat_time, by=.id_new]
  } else {
    data[, .is_after := .next_event_time > .treat_time, by=.id_new]
  }

  # calculate time of first influenza after inclusion time
  data[.is_after==FALSE, .next_event_time := NA]
  data[, .next_event_time := min(.next_event_time, na.rm=TRUE), by=.id_new]
  data[, .is_after := NULL]

  # remove duplicate rows
  data <- unique(data)
  setnames(data, old=".next_event_time", new=next_time_name)

  return(data)
}

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
