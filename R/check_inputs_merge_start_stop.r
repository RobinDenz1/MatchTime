
## check if "col" is a column in all objects in dlist
col_in_all <- function(dlist, col) {
  all(vapply(dlist, FUN=function(x, col){col %in% colnames(x)},
             FUN.VALUE=logical(1), col=col))
}

## return the type of a column in each object in dlist
col_type_dlist <- function(dlist, col) {
  lapply(dlist, FUN=function(x, col){class(x[[col]])}, col=col)
}

## check inputs for merge_td() function
check_inputs_merge_start_stop <- function(dlist, first_time, last_time,
                                          remove_before_first,
                                          remove_after_last,
                                          center_on_first, defaults,
                                          by, start, stop, constant_vars,
                                          event_times, status) {

  # dlist
  stopifnotm(length(dlist) > 0,
             paste0("At least one dataset must be supplied through 'x', 'y'",
                    ", ... or 'dlist'."))
  stopifnotm(all(vapply(dlist, FUN=nrow, FUN.VALUE=numeric(1)) > 0),
             paste0("All datasets supplied through 'x', 'y', ... or ",
                    "'dlist' must have at least one row."))

  # by (id)
  stopifnotm(is_single_character(by),
             "'by' needs to be a single character string.")
  stopifnotm(col_in_all(dlist, col=by),
             paste0("'by' must specify a column defined in all of ",
                    "'x', 'y', ... or all objects in 'dlist'."))

  # start
  stopifnotm(is_single_character(start),
             "'start' needs to be a single character string.")
  stopifnotm(col_in_all(dlist, col=start),
             "'start' must specify a column defined in all supplied datasets.")
  start_types <- unlist(col_type_dlist(dlist, col=start))
  stopifnotm(all(start_types %in% c("integer", "numeric", "Date",
                                    "POSIXct", "POSIXt")),
             paste0("'start' must specify a column of type 'integer',",
                    " 'numeric', 'Date', 'POSIXct' or 'POSIXt' in all ",
                    "supplied datasets."))
  stopifnotm(length(unique(start_types))==1 | all(start_types %in%
                        c("POSIXct", "POSIXt")),
             paste0("The type of the column specified by 'start' must be ",
                    "the same across all supplied datasets."))

  # stop
  stopifnotm(is_single_character(stop),
             "'stop' needs to be a single character string.")
  stopifnotm(col_in_all(dlist, col=stop),
             "'stop' must specify a column defined in all supplied datasets.")
  stop_types <- unlist(col_type_dlist(dlist, col=stop))
  stopifnotm(all(stop_types %in% c("integer", "numeric", "Date",
                                   "POSIXct", "POSIXt")),
             paste0("'stop' must specify a column of type 'integer',",
                    " 'numeric', 'Date', 'POSIXct' or 'POSIXt' in all ",
                    "supplied datasets."))
  stopifnotm(length(unique(stop_types))==1 | all(stop_types %in%
                                                   c("POSIXct", "POSIXt")),
             paste0("The type of the column specified by 'stop' must be ",
                    "the same across all supplied datasets."))

  # start and stop having the same type
  stopifnotm(start_types[1]==stop_types[1],
             paste0("The columns specified by 'start' and 'stop'",
                    " must have the same type."))

  # value names
  all_cnames <- unlist(lapply(dlist, colnames))
  all_cnames <- all_cnames[!all_cnames %in% c(by, start, stop)]
  tab <- table(all_cnames)
  cname_non_unique <- names(tab)[tab > 1]

  stopifnotm(length(cname_non_unique)==0,
             paste0("Columns other than by, start and stop in 'x', 'y', ",
                    "... or 'dlist' must be unique. Found the following",
                    " columns in different datasets:\n",
                    paste(cname_non_unique, collapse=", "), "\n.",
                    " Rename those and run this function again."))

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
                                          by %in% colnames(constant_vars)),
             paste0("'constant_vars' must contain a column named as the ",
                    "string specified by the 'by' argument."))

  # status
  stopifnotm(is_single_character(status),
             "'status' must be a single character string.")

  # event_times
  stopifnotm(is.null(event_times) || is.data.table(event_times),
             "'event_times' must be either NULL or a data.table.")
  if (!is.null(event_times)) {
    stopifnotm(by %in% colnames(event_times),
               paste0("'event_times' must contain a column named as the string",
                      " specified by the 'by' argument."))
    stopifnotm("time" %in% colnames(event_times),
               paste0("'event_times' must contain a column named 'time', ",
                      "containing the event times, if specified."))
    stopifnotm(class(event_times[["time"]])==start_types[1],
               paste0("The 'time' column in 'event_times' must be of the",
                      " same type as 'start' and 'stop'."))
  }

  for (i in seq_len(length(dlist))) {

    # no missing values in start / stop / id
    stopifnotm(!anyNA(dlist[[i]][[by]]),
               "Missing values in 'by' column are not allowed.")
    stopifnotm(!anyNA(dlist[[i]][[start]]),
               "Missing values in 'start' column are not allowed.")
    stopifnotm(!anyNA(dlist[[i]][[stop]]),
               "Missing values in 'stop' column are not allowed.")

    # no intervals of length 0 in dlist
    diff <- dlist[[i]][[stop]] - dlist[[i]][[start]]
    if (any(diff==0)) {
      stop("Intervals of length zero are not supported in 'x', 'y', ..., or ",
           "objects contained in 'dlist'.\nFound in data.table number ", i)
    }
  }

  # check defaults
  all_cnames <- unlist(lapply(dlist, colnames))
  stopifnotm(is.null(defaults) || all(names(defaults) %in% all_cnames),
             paste0("The following column(s) are listed in 'defaults' but",
                    " never appear in any supplied datasets:\n",
                    paste0(names(defaults)[!names(defaults) %in% all_cnames],
                           collapse=", ")))
}
