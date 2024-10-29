
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
as_start_stop <- function(dlist, start_date, end_date, defaults=NULL,
                          id=".id", begin="start", end="stop", value="value") {

  value_dat <- rbindlist(dlist, idcol="dataset")
  unique_ids <- unique(value_dat[[id]])

  # initial data.table
  data <- data.table(.id=rep(value_dat[[id]], 2),
                     start=c(value_dat[[begin]], value_dat[[end]]))

  # add first time
  start_rows <- data.table(.id=unique_ids, start=start_date)
  data <- rbind(data, start_rows)

  # add last time
  end_rows <- data.table(.id=unique_ids, start=end_date)
  data <- rbind(data, end_rows)

  # sort by .id and start & remove duplicates
  setkey(data, .id, start)
  data <- unique(data)
  data <- data[start < end_date, ]

  # create stop
  data[, stop := shift(start, type="lead"), by=.id]

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

  # put in one dataset
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
    data[, (name_stop) := na_locf(eval(parse(text=name_stop))), by=.id]
    data[, (name_value) := na_locf(eval(parse(text=name_value))), by=.id]

    # create indicator if value applies to row
    data[, (name_value) := fifelse(!is.na(eval(parse(text=name_stop))) &
                                     start < eval(parse(text=name_stop)),
                                   eval(parse(text=name_value)), NA, na=NA)]
    data[, (name_stop) := NULL]
  }
  setnames(data, old=var_names_value, new=var_names)

  ## apply defaults, if specified
  if (!is.null(defaults)) {
    for (i in seq_len(length(defaults))) {
      name_i <- names(defaults)[i]
      val_i <- defaults[i]
      data[is.na(eval(parse(text=name_i))), (name_i) := eval(val_i)]
    }
  }

  return(data)
}

## last observation carried forward
na_locf <- function(x) {
  v <- !is.na(x)
  return(c(NA, x[v])[cumsum(v) + 1])
}
