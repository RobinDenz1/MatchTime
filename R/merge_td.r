
# TODO:
# - add support for factor type in "value" column
# - add support for non data.table input in dlist

## function to transform a list of data.tables into a single start-stop
## data.table which may then be used in match_td()
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
#' @importFrom data.table dcast
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table shift
#' @export
merge_td <- function(dlist, first_time=NULL, last_time=NULL,
                     remove_before_first=TRUE, remove_after_last=TRUE,
                     center_on_first=FALSE, defaults=NULL,
                     event_times=NULL, time_to_first_event=FALSE,
                     constant_vars=NULL, id="id", start="start",
                     stop="stop", value="value", status="status",
                     check_inputs=TRUE) {

  . <- .id <- .first_time <- .event_time <- NULL

  if (check_inputs) {
    check_inputs_merge_td(dlist=dlist, first_time=first_time,
                          last_time=last_time,
                          remove_before_first=remove_before_first,
                          remove_after_last=remove_after_last,
                          center_on_first=center_on_first, defaults=defaults,
                          id=id, start=start, stop=stop, value=value,
                          constant_vars=constant_vars, event_times=event_times,
                          status=status)
  }

  # safe value column types for later
  col_types <- lapply(dlist, FUN=function(x){class(x[[value]])})

  # put together all datasets in one
  value_dat <- rbindlist(dlist, idcol="dataset", use.names=TRUE)
  unique_ids <- unique(value_dat[[id]])

  # initial data.table
  data <- data.table(.id=rep(value_dat[[id]], 2),
                     start=c(value_dat[[start]], value_dat[[stop]]))

  # add event times, if specified
  if (!is.null(event_times)) {
    setnames(event_times, old=id, new=".id")
    setnames(event_times, old="time", new="start")
    data <- rbind(data, event_times)
    setnames(event_times, old="start", new=".event_time")
  }

  # add first time
  if (!is.null(first_time)) {
    start_rows <- data.table(.id=unique_ids, start=first_time)
    data <- rbind(data, start_rows)
  }

  # add last time
  if (!is.null(last_time)) {
    end_rows <- data.table(.id=unique_ids, start=last_time)
    data <- rbind(data, end_rows)
  }

  # sort by .id and start & remove duplicates
  setkey(data, .id, start)

  # create stop
  data[, stop := shift(start, type="lead"), by=.id]
  data <- unique(data)
  data <- data[!is.na(stop) & start!=stop]

  # create column names for later
  var_names <- unique(value_dat$dataset)
  var_names_stop <- paste0(stop, "_", var_names)
  var_names_value <- paste0(value, "_", var_names)

  # create one end date for each start + corresponding value
  formula <- stats::as.formula(paste0(id, " + ", start, " ~ dataset"))
  value_dat <- dcast(value_dat, formula=formula, value.var=c(stop, value),
                     drop=TRUE)
  setnames(value_dat, old=c(id, start), new=c(".id", "start"))

  # add this info to start-stop intervals
  data <- merge.data.table(data, value_dat, by=c(".id", "start"),
                           all.x=TRUE, all.y=FALSE)
  rm(value_dat)

  # fill up variable info where appropriate
  for (i in seq_len(length(var_names_stop))) {
    name_stop <- var_names_stop[i]
    name_value <- var_names_value[i]

    # apply LOCF to both stop and value
    set_na_locf_by_id(data=data, name=name_stop)
    set_na_locf_by_id(data=data, name=name_value)

    # update value accordingly
    data[, (name_value) := fifelse(!is.na(get(name_stop)) &
                                     start < get(name_stop),
                                   get(name_value), NA, na=NA)]
    data[, (name_stop) := NULL]
  }
  setnames(data, old=var_names_value, new=var_names)

  # remove periods before start
  if (remove_before_first & !is.null(first_time)) {
    data <- data[start >= first_time, ]
  }

  # remove periods after stop
  if (remove_after_last & !is.null(last_time)) {
    data <- data[start < last_time, ]
  }

  # parse columns back to original class
  set_col_classes(data=data, col_types=col_types)

  # apply defaults, if specified
  if (!is.null(defaults)) {
    for (i in seq_len(length(defaults))) {
      name_i <- names(defaults)[i]
      val_i <- defaults[i]
      data[is.na(eval(parse(text=name_i))), (name_i) := eval(val_i)]
    }
  }

  # center output on first time, if specified
  if (center_on_first & !is.null(first_time)) {
    data[, start := as.vector(start - first_time)]
    data[, stop := as.vector(stop - first_time)]
  } else if (center_on_first) {
    data[, .first_time := min(start), by=.id]
    data[, start := as.vector(start - .first_time)]
    data[, stop := as.vector(stop - .first_time)]
    data[, .first_time := NULL]
  }

  # add constant variables, if specified
  if (!is.null(constant_vars)) {
    setnames(constant_vars, id, ".id")
    data <- merge.data.table(data, constant_vars, by=".id", all.x=TRUE,
                             all.y=FALSE)
  }

  # add event indicator, if event times were supplied
  if (!is.null(event_times)) {

    if (time_to_first_event) {
      event_times <- event_times[, .(.event_time = min(.event_time)), by=.id]
    }

    data <- merge.data.table(data, event_times, by=".id", all.x=TRUE,
                             allow.cartesian=TRUE)
    data[, (status) := any(stop==.event_time), by=c(".id", "start")]
    data[is.na(eval(status)), (status) := FALSE]

    if (time_to_first_event) {
      data <- data[stop <= .event_time | is.na(.event_time)]
      data[, .event_time := NULL]
    } else {
      data[, .event_time := NULL]
      data <- unique(data)
    }
  }

  return(data)
}

## applies na_locf() by group in a data.table and changes the column in place
## NOTE: this seems weird because we could just use one na_locf() call per .id
##       but it is quite a bit faster this way, because it removes the overhead
##       of calling the na_locf() function over and over again
#' @importFrom data.table :=
#' @importFrom data.table .N
set_na_locf_by_id <- function(data, name) {

  .id <- leading_na <- NULL

  # identify first non-NA by .id
  data[!is.na(get(name)), leading_na := seq_len(.N)==1, by=.id]
  data[is.na(leading_na), leading_na := FALSE]

  # cumsum() by .id, making everything == 0 a leading NA value
  data[, leading_na := cumsum(leading_na), by=.id]

  # call na_locf() a single time
  data[, (name) := na_locf(get(name))]

  # set leading NAs that were wrongly filled up back to NA
  data[leading_na==0, (name) := NA]
  data[, leading_na := NULL]
}

## last observation carried forward
na_locf <- function(x) {
  v <- !is.na(x)
  return(c(NA, x[v])[cumsum(v) + 1])
}

## given a data.table and a named list of column classes, parse
## the columns back to that class
set_col_classes <- function(data, col_types) {

  for (i in seq_len(length(col_types))) {
    name_i <- names(col_types)[[i]]
    type_i <- col_types[[i]]

    if (type_i=="logical") {
      data[, (name_i) := as.logical(get(name_i))]
    } else if (type_i=="numeric") {
      data[, (name_i) := as.numeric(get(name_i))]
    } else if (type_i=="character") {
      data[, (name_i) := as.character(get(name_i))]
    } else if (type_i=="integer") {
      data[, (name_i) := as.integer(get(name_i))]
    }
  }
}
