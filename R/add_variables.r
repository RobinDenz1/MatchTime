
## add time-to-event outcome to matched data
#' @importFrom data.table :=
add_tte_outcome <- function(id, time, data, d_event, d_covars,
                            censor_pairs) {

  .next_treat_time <- .treat_time <- .next_event_time <- status <-
    event_time <- pair_id_event_time <- .strata <- NULL

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

  # get maximum follow-up time per person
  d_longest <- d_covars[, (.max_t = max(stop)), by=eval(id)]
  colnames(d_longest) <- c(".id", ".max_t")
  data <- merge(data, d_longest, by=id, all.x=TRUE)

  # calculate corresponding event time
  data[, event_time := pmin(.next_treat_time, .next_event_time,
                            .max_t - .treat_time, na.rm=TRUE)]
  data[, .max_t := NULL]

  # if specified and a control is censored because it became a case later,
  # also censor the corresponding pair to which it is a control at the same time
  # TODO:
  #   - this does not work with ratio > 1
  #   - there is weird stuff going on here that needs to be investigated
  if (censor_pairs) {

    # identify all such cases
    d_cens <- data[.next_treat_time < .next_event_time]
    d_cens <- d_cens[, c("pair_id", "event_time"), with=FALSE]
    colnames(d_cens) <- c("pair_id", "pair_id_event_time")

    # merge them to data, update variables accordingly
    data <- merge(data, d_cens, by="pair_id", all.x=TRUE)
    data[pair_id_event_time < event_time, status := FALSE]
    data[pair_id_event_time < event_time, event_time := pair_id_event_time]
    data[, pair_id_event_time := NULL]
  }
  data[, .strata := NULL]

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
