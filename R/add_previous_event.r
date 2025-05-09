
## given a data.table containing event times per person and a duration of
## that event, adds an indicator to the matched data which is TRUE if the
## event was currently going on when the person got matched and FALSE otherwise
#' @importFrom data.table :=
#' @importFrom data.table fifelse
#' @export
add_previous_event <- function(x, data, id=x$id, time=x$time, duration,
                               include_same_t=FALSE, units="auto",
                               name=".prev_event") {

  x <- add_previous_event_count(x=x, data=data, id=id, time=time,
                                duration=duration,
                                include_same_t=include_same_t,
                                name=name, units=units)
  x$data[, (name) := fifelse(x$data[[name]] > 0, TRUE, FALSE)]

  return(x)
}
