
## transforms a data.table in the long format to a data.table in
## the start / stop format
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table shift
#' @importFrom data.table setcolorder
#' @importFrom data.table setnames
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .N
#' @importFrom data.table copy
#' @export
long2start_stop <- function(data, id, time, varying, start_name="start",
                            stop_name="stop") {

  start <- .is_equal_to_next <- .is_last <- NULL

  # transform to data.table if needed
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_long2start_stop(data=data, id=id, time=time,
                               varying=varying, start_name=start_name,
                               stop_name=stop_name)

  setkeyv(data, c(id, time))

  # get indicator for first occurence per id and final time
  data[, .is_last := seq_len(.N)==.N, by=eval(id)]

  # identify rows that changed
  data[, .is_equal_to_next := check_next_row_equal(.SD), by=eval(id),
       .SDcols=varying]
  data[, .is_equal_to_next := shift(.is_equal_to_next, type="lag"),
       by=eval(id)]

  # remove un-needed rows
  data <- data[is.na(.is_equal_to_next) | .is_equal_to_next==FALSE |
               .is_last==TRUE]
  data[, .is_equal_to_next := NULL]

  # assign start and stop
  setnames(data, old=time, new="start")
  data[, stop := shift(start, type="lead"), by=eval(id)]
  data[is.na(stop), stop := start]

  # remove more rows
  data <- unique(data, by=c(id, "stop", varying))

  # correct last stop
  data[, .is_last := seq_len(.N)==.N, by=eval(id)]
  data[.is_last==TRUE, stop := stop + 1]
  data[, .is_last := NULL]

  # reorder columns
  first_cols <- c(id, "start", "stop", varying)
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))
  setkey(data, NULL)

  if (start_name != "start") {
    setnames(data, old="start", new=start_name)
  }
  if (stop_name != "stop") {
    setnames(data, old="stop", new=stop_name)
  }

  return(data)
}

## check whether the inputs to the long2start_stop function are valid
check_inputs_long2start_stop <- function(data, id, time, varying, start_name,
                                         stop_name) {

  stopifnotm(nrow(data) > 0, "'data' needs to have at least 1 row.")
  stopifnotm(is.character(id) && length(id)==1,
      paste0("'id' has to be a single character string, specifying the unique",
            " person identifier in 'data'."))
  stopifnotm(id %in% colnames(data),
             "The specified 'id' is not a valid column in 'data'.")
  stopifnotm(is.character(data[[id]]) | is.numeric(data[[id]]),
          paste0("The column specified by 'id' must be a character, factor or",
                 " integer variable."))
  stopifnotm(is.character(time) && length(time)==1,
            paste0("'time' has to be a single character string, specifying the",
                   " variable containing points in time in 'data'."))
  stopifnotm(time %in% colnames(data),
             paste0("The column specified by 'time' is not a valid column",
                    "in 'data'."))
  stopifnotm(all(data[[time]] %% 1==0),
             "The variable specified by 'time' may only contain integers.")
  stopifnotm(length(varying) > 0 && is.character(varying),
             paste0("'varying' must be a character vector specifying ",
                    "variables that change over time in 'data'."))
  stopifnotm(all(varying %in% colnames(data)),
        paste0("The following names in 'varying' are not contained in 'data': ",
               paste0(varying[!varying %in% colnames(data)], collapse=", ")))
  stopifnotm(is_single_character(start_name),
             "'start_name' should be a single character string.")
  stopifnotm(is_single_character(stop_name),
             "'stop_name' should be a single character string.")
}
