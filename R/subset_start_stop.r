
## take subset of intervals from start-stop data
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @importFrom data.table setnames
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @export
subset_start_stop <- function(data, first_time, last_time,
                              truncate=TRUE, start="start",
                              stop="stop", na.rm=FALSE) {

  .first_time <- .last_time <- NULL

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  # some input checks
  if (missing(first_time) & missing(last_time)) {
    stop("Either 'first_time' or 'last_time' need to be specified.")
  }

  stopifnotm(is_single_character(start) & start %in% colnames(data),
             paste0("'start' needs to be a single character string ",
                    "specifying a column in 'data'."))
  stopifnotm(is_single_character(stop) & stop %in% colnames(data),
             paste0("'stop' needs to be a single character string ",
                    "specifying a column in 'data'."))
  stopifnotm(missing(first_time) || length(first_time)==1 ||
             length(first_time)==nrow(data),
  "'first_time' must be a single value or have the same length as nrow(data).")
  stopifnotm(missing(last_time) || length(last_time)==1 ||
               length(last_time)==nrow(data),
  "'last_time' must be a single value or have the same length as nrow(data).")
  stopifnotm(is_single_logical(truncate),
             "'truncate' must be either TRUE or FALSE.")

  # rename columns temporarily so that get() always works
  setnames(data, old=c(start, stop), new=c(".start", ".stop"))
  orig_start <- start
  orig_stop <- stop
  start <- ".start"
  stop <- ".stop"

  # remove rows before first_time and after last time
  if (!missing(first_time)) {
    data[, .first_time := first_time]
    data <- data[get(stop) > .first_time | (is.na(.first_time) & !na.rm)]
  }

  if (!missing(last_time)) {
    data[, .last_time := last_time]
    data <- data[get(start) < .last_time | (is.na(.last_time) & !na.rm)]
  }

  # adjust first start and last stop if needed
  if (truncate & !missing(first_time)) {
    data[get(start) < .first_time, (start) := .first_time]
  }

  if (truncate & !missing(last_time)) {
    data[get(stop) > .last_time, (stop) := .last_time]
  }

  # set names back
  setnames(data, old=c(".start", ".stop"), new=c(orig_start, orig_stop))

  if (!missing(first_time)) {
    data[, .first_time := NULL]
  }

  if (!missing(last_time)) {
    data[, .last_time := NULL]
  }

  return(data)
}
