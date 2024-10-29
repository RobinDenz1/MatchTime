
# TODO:
# - write man page
# - write tests (many of them!)
# - write input checks
# - allow no input to start_date and end_date
# - allow id-specific input to start_date and end_date
# - add support for any data type in "value" column
#   (currently only supports numeric, logical by coercing both to numeric)
# - check if it works with integer / continuous times
# - add support for target_event functionality as done in simDAG

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
as_start_stop <- function(dlist, start_date=NULL, end_date=NULL, defaults=NULL,
                          id=".id", begin="start", end="stop", value="value",
                          date_to_int=FALSE) {

  value_dat <- rbindlist(dlist, idcol="dataset")
  unique_ids <- unique(value_dat[[id]])

  # initial data.table
  data <- data.table(.id=rep(value_dat[[id]], 2),
                     start=c(value_dat[[begin]], value_dat[[end]]))

  # add first time
  if (!is.null(start_date)) {
    start_rows <- data.table(.id=unique_ids, start=start_date)
    data <- rbind(data, start_rows)
  }

  # add last time
  if (!is.null(end_date)) {
    end_rows <- data.table(.id=unique_ids, start=end_date)
    data <- rbind(data, end_rows)
  }

  # sort by .id and start & remove duplicates
  setkey(data, .id, start)

  # create stop
  data[, stop := shift(start, type="lead"), by=.id]
  data <- unique(data)
  data <- data[start < end_date, ]

  # create one end date for each start + corresponding value
  formula <- stats::as.formula(paste0(id, " + ", begin, " ~ dataset"))
  value_dat2 <- dcast(value_dat, formula=formula, value.var=end)
  value_dat3 <- dcast(value_dat, formula=formula, value.var=value)

  # change column names
  var_names <- colnames(value_dat2)[3:4]
  var_names_stop <- paste0(var_names, "_stop")
  var_names_value <- paste0(var_names, "_value")
  colnames(value_dat2) <- c(".id", "start", var_names_stop)
  colnames(value_dat3) <- c(".id", "start", var_names_value)

  # put into one dataset
  value_dat <- merge.data.table(value_dat2, value_dat3, by=c(".id", "start"))
  rm(value_dat2, value_dat3)

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

    # create indicator if value applies to row
    data[, (name_value) := fifelse(!is.na(get(name_stop)) &
                                     start < get(name_stop),
                                   get(name_value), NA, na=NA)]
    data[, (name_stop) := NULL]
  }
  setnames(data, old=var_names_value, new=var_names)

  # apply defaults, if specified
  if (!is.null(defaults)) {
    for (i in seq_len(length(defaults))) {
      name_i <- names(defaults)[i]
      val_i <- defaults[i]
      data[is.na(eval(parse(text=name_i))), (name_i) := eval(val_i)]
    }
  }

  # transform date-time objects to integers, if specified
  if (date_to_int) {
    data[, start := as.vector(start - start_date)]
    data[, stop := as.vector(stop - start_date)]
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
