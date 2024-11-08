
## check if "col" is a column in all objects in dlist
col_in_all <- function(dlist, col) {
  all(vapply(dlist, FUN=function(x, col){col %in% colnames(x)},
             FUN.VALUE=logical(1), col=col))
}

## return the type of a column in each object in dlist
col_type_dlist <- function(dlist, col) {
  vapply(dlist, FUN=function(x, col){class(x[[col]])}, FUN.VALUE=character(1),
         col=col)
}

## check inputs for merge_td() function
check_inputs_merge_td <- function(dlist, first_time, last_time,
                                  remove_before_first, remove_after_last,
                                  center_on_first, defaults,
                                  id, start, stop, constant_vars,
                                  event_times, status) {

  # dlist
  stopifnotm(length(dlist) > 1, "'dlist' must contain at least two objects.")

  # id
  stopifnotm(is_single_character(id),
             "'id' needs to be a single character string.")
  stopifnotm(col_in_all(dlist, col=id),
             "'id' must specify a column defined in all objects in 'dlist'.")

  # start
  stopifnotm(is_single_character(start),
             "'start' needs to be a single character string.")
  stopifnotm(col_in_all(dlist, col=start),
             "'start' must specify a column defined in all objects in 'dlist'.")
  start_types <- col_type_dlist(dlist, col=start)
  stopifnotm(all(start_types %in% c("integer", "numeric", "Date")),
             paste0("'start' must specify a column of type 'integer',",
                    " 'numeric' or 'Date' in all objects in 'dlist'."))
  stopifnotm(length(unique(start_types))==1,
             paste0("The type of the column specified by 'start' must be ",
                    "the same across all objects in 'dlist'."))

  # stop
  stopifnotm(is_single_character(stop),
             "'stop' needs to be a single character string.")
  stopifnotm(col_in_all(dlist, col=stop),
             "'stop' must specify a column defined in all objects in 'dlist'.")
  stop_types <- col_type_dlist(dlist, col=stop)
  stopifnotm(all(stop_types %in% c("integer", "numeric", "Date")),
             paste0("'stop' must specify a column of type 'integer',",
                    " 'numeric' or 'Date' in all objects in 'dlist'."))
  stopifnotm(length(unique(stop_types))==1,
             paste0("The type of the column specified by 'stop' must be ",
                    "the same across all objects in 'dlist'."))

  # start and stop having the same type
  stopifnotm(start_types[1]==stop_types[1],
             paste0("The columns specified by 'start' and 'stop'",
                    " must have the same type."))

  # value
  # TODO: there needs to be some variant of this to check all col-types in
  #       the supplied datasets
  #value_types <- col_type_dlist(dlist, col=value)
  #supported_types <- c("integer", "logical", "numeric", "character")
  #stopifnotm(all(value_types %in% supported_types),
  #           paste0("The columns specified by 'value' must be of one of the",
  #                  " supported types:\n integer, logical, numeric, ",
  #                  "character."))

  # first_time
  stopifnotm(is.null(first_time) || length(first_time)==1,
             "'first_time' must be a single value.")
  stopifnotm(is.null(first_time) || class(first_time)==start_types[1],
             paste0("'first_time' must be of the same type as the 'start' ",
                    "and 'stop' columns."))

  # last_time
  stopifnotm(is.null(last_time) || length(last_time)==1,
             "'last_time' must be a single value.")
  stopifnotm(is.null(last_time) || class(last_time)==start_types[1],
             paste0("'last_time' must be of the same type as the 'start' ",
                    "and 'stop' columns."))

  # remove_before_first
  stopifnotm(is_single_logical(remove_before_first),
             "'remove_before_first' must be either TRUE or FALSE.")

  # remove_after_last
  stopifnotm(is_single_logical(remove_after_last),
             "'remove_after_last' must be either TRUE or FALSE.")

  # center_on_first
  stopifnotm(is_single_logical(center_on_first),
             "'center_on_first' must be either TRUE or FALSE.")

  # constant vars
  stopifnotm(is.null(constant_vars) || is.data.table(constant_vars),
             "'constant_vars' must be either NULL or a data.table.")
  stopifnotm(is.null(constant_vars) || (!is.null(constant_vars) &&
                                          id %in% colnames(constant_vars)),
             paste0("'constant_vars' must contain a column named as the ",
                    "string specified by the 'id' argument."))

  # status
  stopifnotm(is_single_character(status),
             "'status' must be a single character string.")

  # event_times
  stopifnotm(is.null(event_times) || is.data.table(event_times),
             "'event_times' must be either NULL or a data.table.")
  if (!is.null(event_times)) {
    stopifnotm(id %in% colnames(event_times),
               paste0("'event_times' must contain a column named as the string",
                      " specified by the 'id' argument."))
    stopifnotm("time" %in% colnames(event_times),
               paste0("'event_times' must contain a column named 'time', ",
                      "containing the event times, if specified."))
    stopifnotm(class(event_times[["time"]])==start_types[1],
               paste0("The 'time' column in 'event_times' must be of the",
                      " same type as 'start' and 'stop'."))
  }

  # no intervals of length 0 in dlist
  for (i in seq_len(length(dlist))) {
    diff <- dlist[[i]][[stop]] - dlist[[i]][[start]]
    if (any(diff==0)) {
      stop("Intervals of length zero are not supported in objects contained",
           " in 'dlist'. Found in data.table named ", names(dlist)[[i]])
    }
  }

  # TODO: check defaults
}
