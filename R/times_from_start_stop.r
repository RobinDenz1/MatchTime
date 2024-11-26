
# extract new event times from data in start-stop format
#' @importFrom data.table :=
#' @importFrom data.table shift
#' @importFrom data.table setkeyv
#' @export
times_from_start_stop <- function(data, id, name, type, start="start",
                                  stop="stop", time_name="time") {

  .temp_shift <- NULL

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_times_from_start_stop(data=data, id=id, name=name, type=type,
                                     start=start, stop=stop,
                                     time_name=time_name)

  if (type!="event") {

    setkeyv(data, c(id, start))

    # identify times of new occurence
    set_shift_by(data, col_in=name, col_out=".temp_shift", type="lag",
                 by=id, fill=FALSE)

    # keep only those rows with new events
    out <- data[.temp_shift==FALSE & get(name)==TRUE
                ][, c(id, start), with=FALSE]
    data[, .temp_shift := NULL]

  } else {
    # simply extract all stop times with name==TRUE
    out <- data[get(name)==TRUE][, c(id, stop), with=FALSE]
  }

  # rename & sort
  colnames(out) <- c(id, time_name)
  setkeyv(out, c(id, time_name))

  return(out)
}

## check inputs for times_from_start_stop() function
check_inputs_times_from_start_stop <- function(data, id, name, type, start,
                                               stop, time_name) {

  stopifnotm(is_single_character(id) && id %in% colnames(data),
     "'id' needs to be a single character specifying a valid column in 'data'.")
  stopifnotm(is_single_character(name) && name %in% colnames(data) &&
               is.logical(data[[name]]),
             paste0("'name' needs to be a single character specifying a ",
                    "logical variable in 'data'."))
  stopifnotm(is_single_character(start) && start %in% colnames(data),
  "'start' needs to be a single character specifying a valid column in 'data'.")
  stopifnotm(is_single_character(stop) && stop %in% colnames(data),
  "'stop' needs to be a single character specifying a valid column in 'data'.")
  stopifnotm(is_single_character(type) && type %in% c("var", "event"),
             "'type' must be either 'var' or 'event'.")
  stopifnotm(is_single_character(time_name),
             "'time_name' must be a single character string.")
}
