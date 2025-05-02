
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

  .first_time <- .last_time <- .start <- .stop <- NULL

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  # some input checks
  if (missing(first_time) & missing(last_time)) {
    stop("Either 'first_time' or 'last_time' need to be specified.",
         call.=FALSE)
  }

  check_inputs_subset_start_stop(data=data, start=start, stop=stop,
                                 first_time=first_time, last_time=last_time,
                                 truncate=truncate)

  # rename columns temporarily so that get() always works
  setnames(data, old=c(start, stop), new=c(".start", ".stop"))

  # remove rows before first_time
  if (!missing(first_time) && !is.null(first_time)) {
    data[, .first_time := first_time]
    data <- data[.stop > .first_time | (is.na(.first_time) & !na.rm)]
  }

  # remove rows after last time
  if (!missing(last_time) && !is.null(last_time)) {
    data[, .last_time := last_time]
    data <- data[.start < .last_time | (is.na(.last_time) & !na.rm)]
  }

  # adjust first start and last stop if needed
  if (truncate && !missing(first_time) && !is.null(first_time)) {
    data[.start < .first_time, .start := .first_time]
  }

  if (truncate && !missing(last_time) && !is.null(last_time)) {
    data[.stop > .last_time, .stop := .last_time]
  }

  # set names back
  setnames(data, old=c(".start", ".stop"), new=c(start, stop))

  if (!missing(first_time) && !is.null(first_time)) {
    data[, .first_time := NULL]
  }

  if (!missing(last_time) && !is.null(last_time)) {
    data[, .last_time := NULL]
  }

  return(data)
}

## check inputs for subset_start_stop() function
check_inputs_subset_start_stop <- function(data, start, stop, first_time,
                                           last_time, truncate) {

  stopifnotm(is_single_character(start) & start %in% colnames(data),
             paste0("'start' needs to be a single character string ",
                    "specifying a column in 'data'."))
  stopifnotm(is_single_character(stop) & stop %in% colnames(data),
             paste0("'stop' needs to be a single character string ",
                    "specifying a column in 'data'."))
  stopifnotm(missing(first_time) || is.null(first_time) ||
               length(first_time)==1 || length(first_time)==nrow(data),
   "'first_time' must be a single value or have the same length as nrow(data).")
  stopifnotm(missing(last_time) || is.null(last_time) ||
               length(last_time)==1 || length(last_time)==nrow(data),
    "'last_time' must be a single value or have the same length as nrow(data).")
  stopifnotm(is_single_logical(truncate),
             "'truncate' must be either TRUE or FALSE.")
}
