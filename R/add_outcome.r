
## add time-to-event outcome to matched data
#' @importFrom data.table :=
#' @importFrom data.table fifelse
#' @importFrom data.table merge.data.table
#' @importFrom data.table copy
#' @importFrom data.table setnames
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @export
add_outcome <- function(x, d_event, censor_at_treat=TRUE,
                        censor_pairs=FALSE, units="auto",
                        id=x$id, time=x$time,
                        event_time_name=".event_time",
                        status_name=".status", copy_data=TRUE) {

  .next_treat_time <- .treat_time <- .next_event_time <- .status <-
    .time_to_max_t <- .max_t <- .event_time <- .artificial_cens_time <-
    .id_pair <- NULL

  if (copy_data) {
    x <- copy(x)
  }

  if (!is.data.table(d_event)) {
    d_event <- as.data.table(d_event)
  } else {
    d_event <- copy(d_event)
  }

  check_inputs_add_outcome(x=x, d_event=d_event, id=id, time=time,
                           censor_at_treat=censor_at_treat,
                           censor_pairs=censor_pairs,
                           event_time_name=event_time_name,
                           status_name=status_name)

  # make names consistent over datasets
  if (time!=x$time) {
    setnames(d_event, old=time, new=x$time)
  }
  if (id!=x$id) {
    setnames(d_event, old=id, new=x$id)
  }

  # add event information
  x$data <- add_next_event_time(data=x$data, d_event=d_event, id=x$id,
                                time=x$time, include_same_t=TRUE,
                                next_time_name=".next_event_time")

  # shift times according to start
  if (all(class(x$data$.treat_time) %in% c("Date", "POSIXct", "POSIXlt"))) {
    x$data[, .time_to_next_treat := as.numeric(difftime(.next_treat_time,
                                                     .treat_time,
                                                     units=units))]
    x$data[, .time_to_next_event := as.numeric(
      difftime(.next_event_time, .treat_time, units=units))]
  } else {
    x$data[, .time_to_next_treat := .next_treat_time - .treat_time]
    x$data[, .time_to_next_event := .next_event_time - .treat_time]
  }

  # get event status indicator
  x$data[, .status := FALSE]

  if (censor_at_treat){
    x$data[is.na(.time_to_next_treat) & !is.na(.time_to_next_event),
           .status := TRUE]
    x$data[.time_to_next_treat > .time_to_next_event, .status := TRUE]
  } else {
    x$data[!is.na(.time_to_next_event), .status := TRUE]
  }

  # add maximum follow-up time per person
  x$data <- merge.data.table(x$data, x$d_longest, by=x$id, all.x=TRUE)

  # time to max_t
  if (all(class(x$data$.treat_time) %in% c("Date", "POSIXct", "POSIXlt"))) {
    x$data[, .time_to_max_t := as.numeric(difftime(.max_t,
                                                   .treat_time,
                                                   units=units))]
  } else {
    x$data[, .time_to_max_t := .max_t - .treat_time]
  }

  # calculate corresponding event time
  if (censor_at_treat) {
    x$data[, .event_time := pmin(.time_to_next_treat, .time_to_next_event,
                                 .time_to_max_t, na.rm=TRUE)]
  } else {
    x$data[, .event_time := pmin(.time_to_next_event, .time_to_max_t,
                                 na.rm=TRUE)]
  }
  x$data[, .max_t := NULL]

  # if specified and a control is censored because it became a case later,
  # also censor the corresponding pair to which it is a control at the same time
  if (censor_pairs && censor_at_treat) {

    # needs to be the same type for older versions of data.table
    if (all(class(x$data[[time]]) %in% c("Date", "POSIXct", "POSIXlt"))) {
      max_time <- as.Date(Inf)
    } else {
      max_time <- Inf
    }

    # new variable that is Inf if no artificial censoring is needed or the
    # minimum of the artificial censoring times inside matched pair groups
    x$data[, .artificial_cens_time := fifelse(.time_to_next_treat==.event_time,
                                            .event_time, max_time, na=max_time)]
    x$data[, .artificial_cens_time := min(.artificial_cens_time), by=.id_pair]

    # update the data accordingly
    x$data[.artificial_cens_time < .event_time, .status := FALSE]
    x$data[.artificial_cens_time < .event_time,
           .event_time := .artificial_cens_time]
    x$data[, .artificial_cens_time := NULL]
  }

  x$data[, .time_to_max_t := NULL]
  x$data[, .next_event_time := NULL]
  x$data[, .time_to_next_event := NULL]
  x$data[, .time_to_next_treat := NULL]

  setnames(x$data, old=c(".event_time", ".status"),
           new=c(event_time_name, status_name))

  # update vector of names already added
  x$info$added_events <- c(x$info$added_events, event_time_name, status_name)

  return(x)
}

## input checks for the add_outcome() function
check_inputs_add_outcome <- function(x, id, time, d_event, censor_at_treat,
                                     censor_pairs, event_time_name,
                                     status_name) {

  stopifnotm(inherits(x, "MatchTD"),
             paste0("'x' must be a 'MatchTD' object created using the",
                    " match_td() function."))
  stopifnotm(is_single_logical(censor_pairs),
             "'censor_pairs' must be either TRUE or FALSE.")
  stopifnotm(is_single_logical(censor_at_treat),
             "'censor_at_treat' must be either TRUE or FALSE.")
  stopifnotm(is_single_character(event_time_name),
             "'event_time_name' must be a single character string.")
  stopifnotm(is_single_character(event_time_name),
             "'status_name' must be a single character string.")
  stopifnotm(is_single_character(id),
             "'id' must be a single character string.")
  stopifnotm(is_single_character(time),
             "'time' must be a single character string.")

  # d_event
  stopifnotm(ncol(d_event)==2,
             paste0("'d_event' should only have two columns, containing the ",
                    "case id and the time."))
  stopifnotm(id %in% colnames(d_event),
             "'id' must specify a valid column in 'd_event'.")
  stopifnotm(time %in% colnames(d_event),
             "'time' must specify a valid column in 'd_event'.")
}
