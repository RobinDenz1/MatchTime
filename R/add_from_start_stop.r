
## add a variable to a match_time object using start-stop data as input
#' @importFrom data.table copy
#' @importFrom data.table as.data.table
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
#' @importFrom data.table setkeyv
#' @export
add_from_start_stop <- function(x, data, variable, id=x$id,
                                start="start", stop="stop",
                                default=NA) {
  .treat_time <- .var <- NULL

  x <- copy(x)

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  check_inputs_add_from_start_stop(x=x, data=data, variable=variable,
                                   id=id, start=start, stop=stop,
                                   default=default)

  setnames(data, old=variable, new=".var")

  if (id!=x$id) {
    setnames(data, old=id, new=x$id)
  }

  d_id_treat <- x$data[, c(x$id, ".id_new", ".treat_time"), with=FALSE]
  data <- merge(data, d_id_treat, by=x$id, all.x=TRUE, allow.cartesian=TRUE)
  data <- subset(data, .treat_time >= start & .treat_time < stop)
  data <- data[, c(".id_new", ".var"), with=FALSE]
  x$data <- merge(x$data, data, by=".id_new", all.x=TRUE)

  x$data[is.na(.var), .var := default]

  setnames(x$data, old=".var", new=variable)
  setkeyv(x$data, x$id)

  return(x)
}

## check inputs for the add_from_start_stop() function
check_inputs_add_from_start_stop <- function(x, data, variable, id, start,
                                             stop, default) {
  stopifnotm(inherits(x, "match_time"),
             paste0("'x' must be a 'match_time' object created using the",
                    " match_time() function."))
  stopifnotm(is_single_character(id),
             "'id' must be a single character string.")
  stopifnotm(is_single_character(start),
             "'start' must be a single character string.")
  stopifnotm(is_single_character(stop),
             "'stop' must be a single character string.")
  stopifnotm(is_single_character(variable),
             "'variable' must be a single character string.")
  stopifnotm(id %in% colnames(data),
             "'id' must specify a valid column in 'data'.")
  stopifnotm(start %in% colnames(data),
             "'start' must specify a valid column in 'data'.")
  stopifnotm(stop %in% colnames(data),
             "'stop' must specify a valid column in 'data'.")
  stopifnotm(variable %in% colnames(data),
             "'variable' must specify a valid column in 'data'.")
  stopifnotm(length(default)==1, "'default' must be of length 1.")
}
