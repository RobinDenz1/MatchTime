
## given a data.table containing event times per person and a duration of
## that event, adds an indicator to the matched data which is TRUE if the
## event was currently going on when the person got matched and FALSE otherwise
#' @importFrom data.table :=
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
#' @export
add_previous_event <- function(x, data, id=x$id, time=x$time, duration,
                               include_same_t=TRUE, name=".prev_event") {

  .in_risk <- .treat_time <- .time <- .id_new <- .next_treat_time <- NULL

  x <- copy(x)

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_add_next_time(x=x, data=data, id=id, time=time,
                             include_same_t=include_same_t,
                             name=name)

  # make names consistent over datasets
  setnames(data, old=time, new=".time")

  if (id!=x$id) {
    setnames(data, old=id, new=x$id)
  }

  # merge to matched data, creating new rows
  x$data <- merge.data.table(x$data, data, by=x$id, all.x=TRUE)

  # check if .treat_time is in any of the previous risk periods by event
  # NOTE: the way I simulated the data this argument should be TRUE for
  #       time-dependent covariates, but FALSE for outcomes
  if (include_same_t) {
    x$data[, .in_risk := .treat_time < (.time + eval(duration)) &
             .treat_time >= .time]
  } else {
    x$data[, .in_risk := .treat_time < (.time + eval(duration)) &
             .treat_time > .time]
  }

  x$data[is.na(.in_risk), .in_risk := FALSE]
  x$data[, .in_risk := any(.in_risk), by=.id_new]
  x$data[, .time := NULL]

  # remove duplicate rows
  x$data <- unique(x$data)
  setnames(x$data, old=".in_risk", new=name)

  return(x)
}
