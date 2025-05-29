
## transform start-stop format data into the long-format
#' @importFrom data.table is.data.table
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table .I
#' @importFrom data.table setkeyv
#' @importFrom data.table first
#' @importFrom data.table merge.data.table
#' @export
start_stop2long <- function(data, id, events=NULL, start="start", stop="stop",
                            fill_gaps=FALSE, include_last_t=FALSE,
                            time_name="time", ...) {

  .width <- .start <- .stop <- .max_stop <- NULL

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_start_stop2long(data=data, id=id, start=start, stop=stop,
                               fill_gaps=fill_gaps,
                               include_last_t=include_last_t,
                               time_name=time_name, events=events)

  if (fill_gaps) {
    data <- fill_gaps_start_stop(data=data, id=id, start=start, stop=stop,
                                 ...)
  }

  setnames(data, old=c(start, stop), new=c(".start", ".stop"))

  if (include_last_t) {
    data[, .max_stop := max(.stop), by=eval(id)]
    d_last <- data[.stop==.max_stop]
    d_last[, .start := NULL]
    setnames(d_last, old=".stop", new=time_name)
    data[, .max_stop := NULL]
    d_last[, .max_stop := NULL]
  }

  # extract event times, if needed
  if (!is.null(events)) {
    d_events <- vector(mode="list", length=length(events))
    for (i in seq_len(length(events))) {
      d_i <- times_from_start_stop(data=data, id=id, name=events[i],
                                   type="event", start=".start",
                                   stop=".stop", time_name=time_name)
      d_i[, (events[i]) := TRUE]
      d_events[[i]] <- d_i
    }
  }

  # repeat rows as many times as needed
  data[, .width := .stop - .start]
  data <- data[rep(data[, .I], data$.width)]

  # create .time variable
  data[, (time_name) := seq(first(.start), first(.stop)-1), by=c(id, ".start")]

  # adjust events
  if (!is.null(events)) {
    data[, c(events) := NULL]

    for (i in seq_len(length(events))) {
      data <- merge.data.table(data, d_events[[i]], by=c(id, time_name),
                               all.x=TRUE)
      data[is.na(get(events[i])), (events[i]) := FALSE]
    }
  }

  # remove columns no longer needed
  data[, .width := NULL]
  data[, .start := NULL]
  data[, .stop := NULL]

  if (include_last_t) {
    data <- rbind(data, d_last)
  }

  # sort by id, time
  setkeyv(data, c(id, time_name))

  return(data)
}

## check inputs for start_stop2long() function
check_inputs_start_stop2long <- function(data, id, start, stop,
                                         fill_gaps, include_last_t,
                                         time_name, events) {

  stopifnotm(is_single_character(id) & id %in% colnames(data),
    "'id' must be a single character string, specifying a column in 'data'.")
  stopifnotm(is_single_character(start) & start %in% colnames(data),
    "'start' must be a single character string, specifying a column in 'data'.")
  stopifnotm(is_single_character(stop) & stop %in% colnames(data),
    "'stop' must be a single character string, specifying a column in 'data'.")

  stopifnotm(is_single_logical(fill_gaps),
    "'fill_gaps' should be either TRUE or FALSE.")
  stopifnotm(is_single_logical(include_last_t),
             "'include_last_t' should be either TRUE or FALSE.")
  stopifnotm(is_single_character(time_name),
             "'time_name' should be a single character string.")
  stopifnotm(length(unique(c(id, start, stop, time_name)))==4,
  "'id', 'start', 'stop' and 'time_name' cannot have the same values.")

  if (!is.null(events)) {
    for (i in seq_len(length(events))) {
      stopifnotm(is_single_character(events[i]) &&
                 events[i] %in% colnames(data) &&
                 is.logical(data[[events[i]]]),
                 paste0("'events' must be NULL or a character vector ",
                        "specifying one or more logical columns in 'data'."))
    }
  }
}
