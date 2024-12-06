
## add the time of occurrence of some event or similar thing after
## inclusion to a match_td object
#' @importFrom data.table copy
#' @export
add_next_time <- function(x, data, id=x$id, time=x$time,
                          include_same_t=TRUE, name=".next_time") {

  x <- copy(x)

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_add_variable(x=x, data=data, id=id, time=time,
                            include_same_t=include_same_t,
                            name=name)

  # make names consistent over datasets
  if (time!=x$time) {
    setnames(data, old=time, new=x$time)
  }
  if (id!=x$id) {
    setnames(data, old=id, new=x$id)
  }

  # add the information
  x$data <- add_next_time_data(data=x$data, d_event=data, id=x$id, time=x$time,
                               include_same_t=include_same_t,
                               name=name)
  x$info$added_next_time <- c(x$info$added_next_time, name)

  return(x)
}

## given the current matched data and a data.table containing none, one or
## multiple events per person, add the next event after .treat_time
#' @importFrom data.table :=
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
add_next_time_data <- function(data, d_event, id, time, include_same_t=TRUE,
                               name) {

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
  setnames(data, old=".next_event_time", new=name)

  return(data)
}
