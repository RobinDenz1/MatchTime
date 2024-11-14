
## add time-to-event outcome to matched data
#' @importFrom data.table :=
#' @importFrom data.table fifelse
add_tte_outcome <- function(id, time, data, d_event, d_longest,
                            censor_at_treat, censor_pairs) {

  .next_treat_time <- .treat_time <- .next_event_time <- status <-
    event_time <- pair_id_event_time <- .artificial_cens_time <- pair_id <-
    .max_t <- NULL

  # add event information
  data <- add_next_event_time(data=data, d_event=d_event, id=id, time=time,
                              include_same_t=TRUE)

  # shift times according to start
  data[, .next_treat_time := .next_treat_time - .treat_time]
  data[, .next_event_time := .next_event_time - .treat_time]

  # get event status indicator
  data[, status := FALSE]
  data[is.na(.next_treat_time) & !is.na(.next_event_time), status := TRUE]
  data[.next_treat_time > .next_event_time, status := TRUE]

  # add maximum follow-up time per person
  data <- merge(data, d_longest, by=id, all.x=TRUE)

  # calculate corresponding event time
  if (censor_at_treat) {
    data[, event_time := pmin(.next_treat_time, .next_event_time,
                              as.vector(.max_t - .treat_time), na.rm=TRUE)]
  } else {
    data[, event_time := pmin(.next_event_time, as.vector(.max_t - .treat_time),
                              na.rm=TRUE)]
  }
  data[, .max_t := NULL]

  # if specified and a control is censored because it became a case later,
  # also censor the corresponding pair to which it is a control at the same time
  if (censor_pairs && censor_at_treat) {

    # needs to be the same type for older versions of data.table
    if (all(class(data[[time]]) %in% c("Date", "POSIXct", "POSIXlt"))) {
      max_time <- as.Date(Inf)
    } else {
      max_time <- Inf
    }

    # new variable that is Inf if no artificial censoring is needed or the
    # minimum of the artificial censoring times inside matched pair groups
    data[, .artificial_cens_time := fifelse(.next_treat_time == event_time,
                                            event_time, max_time, na=max_time)]
    data[, .artificial_cens_time := min(.artificial_cens_time), by=pair_id]

    # update the data according to this
    data[.artificial_cens_time < event_time, status := FALSE]
    data[.artificial_cens_time < event_time,
         event_time := .artificial_cens_time]
    data[, .artificial_cens_time := NULL]
  }

  return(data)
}

## given the current matched data and a data.table containing none, one or
## multiple events per person, add the next event after .treat_time
#' @importFrom data.table :=
add_next_event_time <- function(data, d_event, id, time, include_same_t=TRUE) {

  is_after <- .next_event_time <- .treat_time <- .id_new <- .in_risk <- NULL

  # merge to matched data, creating new rows
  colnames(d_event)[colnames(d_event)==time] <- ".next_event_time"
  data <- merge(data, d_event, by=id, all.x=TRUE, allow.cartesian=TRUE)

  # check if event is after inclusion time
  # NOTE: the way I simulated the data this argument should be TRUE for
  #       outcomes, but FALSE for time-dependent variables
  #       (although it is unclear why it would be used for the latter)
  if (include_same_t) {
    data[, is_after := .next_event_time >= .treat_time, by=.id_new]
  } else {
    data[, is_after := .next_event_time > .treat_time, by=.id_new]
  }

  # calculate time of first influenza after inclusion time
  data[is_after==FALSE, .next_event_time := NA]
  data[, .next_event_time := min(.next_event_time, na.rm=TRUE), by=.id_new]
  data[, is_after := NULL]

  # remove duplicate rows
  data <- unique(data)

  return(data)
}

## given a data.table containing event times per person and a duration of
## that event, adds an indicator to the matched data which is TRUE if the
## event was currently going on when the person got matched and FALSE otherwise
# TODO: .id_new should maybe be an argument
#' @importFrom data.table :=
add_previous_event_time <- function(data, d_prev, id, time, duration,
                                    name, include_same_t=FALSE) {

  .in_risk <- .treat_time <- .prev_time <- .id_new <- .next_treat_time <- NULL

  # merge to matched data, creating new rows
  colnames(d_prev)[colnames(d_prev)==time] <- ".prev_time"
  data <- merge(data, d_prev, by=id, all.x=TRUE)

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
  colnames(data)[colnames(data)==".in_risk"] <- name

  return(data)
}

## removes all times after first treatment onset
#' @importFrom data.table :=
remove_after_treat <- function(data, time, overlap=FALSE,
                               remove_time_var=TRUE) {

  start <- NULL

  # if overlapping ones are supplied, simply re-transform to non-overlapping
  # start-stop format, perform the transformation and re-add the + 1
  if (overlap) {
    data[, stop := stop - 1]
  }

  # needs to be the same type for older versions of data.table
  if (all(class(data[[time]]) %in% c("Date", "POSIXct", "POSIXlt"))) {
    max_time <- as.Date(Inf)
  } else {
    max_time <- Inf
  }

  # set NA to maximum possible time
  suppressWarnings(
    data[, (time) := fifelse(is.na(get(time)), max_time, get(time))]
  )

  # remove row if start is after treatment
  data <- data[!(start > get(time))]

  # change stop accordingly if it is after treatment onset
  data[stop > get(time), stop := get(time)]

  if (overlap) {
    data[, stop := stop + 1]
  }

  if (remove_time_var) {
    data[, (time) := NULL]
  }

  return(data)
}

## removes all times before first treatment onset
#' @importFrom data.table :=
remove_before_treat <- function(data, time, overlap=FALSE,
                                remove_time_var=TRUE) {

  start <- NULL

  # if overlapping ones are supplied, simply re-transform to non-overlapping
  # start-stop format, perform the transformation and re-add the + 1
  if (overlap) {
    data[, stop := stop - 1]
  }

  # needs to be the same type for older versions of data.table
  if (all(class(data[[time]]) %in% c("Date", "POSIXct", "POSIXlt"))) {
    min_time <- as.Date(-Inf)
  } else {
    min_time <- -Inf
  }

  # set NA to maximum possible time
  suppressWarnings(
    data[, (time) := fifelse(is.na(get(time)), min_time, get(time))]
  )

  # remove row if start is before treatment
  data <- data[!(stop < get(time))]

  # change start accordingly if it is after treatment onset
  data[start < get(time), start := get(time)]

  if (overlap) {
    data[, stop := stop + 1]
  }

  if (remove_time_var) {
    data[, (time) := NULL]
  }

  return(data)
}

## counts all events that happened "duration" days before time
add_previous_event_count <- function(data, d_prev, id, id_new, time, duration,
                                     name, include_same_t=FALSE) {

  . <- .prev_time <- count <- NULL

  colnames(d_prev)[colnames(d_prev)==time] <- ".prev_time"
  data <- merge(data, d_prev, by=id, all.x=TRUE)

  data[, diff := as.vector(.prev_time - get(time))]

  if (include_same_t) {
    out <- data[, .(count = sum(diff <= 0 & diff >= -duration)),
                by=eval(id_new)]
  } else {
    out <- data[, .(count = sum(diff < 0 & diff >= -duration)),
                by=eval(id_new)]
  }
  out[is.na(count), count := 0]

  colnames(out)[colnames(out)=="count"] <- name

  return(out)
}
