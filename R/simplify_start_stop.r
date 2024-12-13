
## combines multiple time-intervals where no values change
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table shift
#' @importFrom data.table copy
#' @importFrom data.table setkeyv
#' @importFrom data.table .SD
#' @importFrom data.table .N
#' @export
simplify_start_stop <- function(data, id, start="start", stop="stop", cols,
                                remove_other_cols=TRUE) {

  .is_equal_to_next <- .temp_shift <- .first <- .first_start <- NULL

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  # if not specified, use all available columns other than
  # id, start and stop
  if (missing(cols)) {
    cols <- colnames(data)[!colnames(data) %in% c(id, start, stop)]
  }

  check_inputs_simplify_start_stop(data=data, id=id, start=start, stop=stop,
                                   remove_other_cols=remove_other_cols,
                                   cols=cols)

  # sort by id, start
  setkeyv(data, c(id, start))

  data[, .first_start := min(start), by=eval(id)]

  # remove rows that are equal to the next row
  data[, .is_equal_to_next := check_next_row_equal(.SD), .SDcols=cols,
       by=eval(id)]
  data <- data[is.na(.is_equal_to_next) | .is_equal_to_next==FALSE]
  data[, .is_equal_to_next := NULL]

  # adjust start accordingly
  set_shift_by(data, col_in=stop, col_out=".temp_shift", by=id, type="lag")
  data[.temp_shift < eval(start), (start) := .temp_shift]
  data[, .temp_shift := NULL]

  # also adjust first row per person, if needed
  data[, .first := seq_len(.N)==1, by=eval(id)]
  data[.first==TRUE, start := .first_start]
  data[, .first := NULL]
  data[, .first_start := NULL]

  if (remove_other_cols) {
    other_cols <- colnames(data)[!colnames(data) %in% c(id, start, stop, cols)]

    if (length(other_cols) > 0) {
      data[, (other_cols) := NULL]
    }
  }

  setkeyv(data, c(id, start))

  return(data)
}

## check if each row is equal to the next row in a data.table
#' @importFrom data.table shift
check_next_row_equal <- function(x) {
  shift_x <- shift(x, n=1, type="lead", fill=NA)
  return(rowSums(x==shift_x)==ncol(x))
}

## check inputs for simplify_start_stop() function
#' @importFrom data.table :=
check_inputs_simplify_start_stop <- function(data, id, start, stop, cols,
                                             remove_other_cols) {

  # id
  stopifnotm(is_single_character(id),
             "'id' must be a single character string.")
  stopifnotm(id %in% colnames(data),
             "'id' must be a valid column in 'data'.")

  # start
  stopifnotm(is_single_character(start),
             "'start' must be a single character string.")
  stopifnotm(start %in% colnames(data),
             "'start' must be a valid column in 'data'.")

  # stop
  stopifnotm(is_single_character(stop),
             "'stop' must be a single character string.")
  stopifnotm(stop %in% colnames(data),
             "'stop' must be a valid column in 'data'.")

  # no intervals of length <= 0
  stopifnotm(all(data[[start]] <= data[[stop]]),
             "'start' must always be greater than 'stop'.")

  # full, overlapping intervals
  .temp_check <- NULL
  set_shift_by(data, col_out=".temp_check", col_in=stop, type="lag",
               by=id)
  stopifnotm(all(data[[start]]==data$.temp_check | is.na(data$.temp_check)),
             paste0("Full intervals must be supplied, meaning that 'start'",
                    " must always be equal to the next value of 'stop'",
                    " per 'id'."))
  data[, .temp_check := NULL]
}
