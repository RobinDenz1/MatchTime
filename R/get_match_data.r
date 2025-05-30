
## removes all cases that were not matched to >= n_required controls
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @export
get_match_data <- function(object, remove_unmatched=TRUE,
                           n_required=object$info$ratio) {

  .id_pair <- .count_id_pair <- .fully_matched <- NULL

  stopifnotm(inherits(object, "match_time"),
    paste0("'object' must be a match_time object created using the ",
           "match_time() function."))
  stopifnotm(is_single_logical(remove_unmatched),
             "'remove_unmatched' must be either TRUE or FALSE.")
  stopifnotm(length(n_required)==1 && is.numeric(n_required) &&
               n_required >= 1,
             "'n_required' should be a single integer.")

  if (n_required != object$info$ratio && remove_unmatched &&
      !".id_pair" %in% colnames(object$data)) {
    warning("Argument 'n_required' ignored, because there are no pairs",
            " to which this argument could be applied.")
  }

  data <- copy(object$data)

  if (remove_unmatched & (n_required==object$info$ratio ||
                          !".id_pair" %in% colnames(data))) {
    data <- data[.fully_matched==TRUE]
    data[, .fully_matched := NULL]
  } else if (remove_unmatched) {
    data <- remove_unmatched(data=data, n_required=n_required)
    data[, .fully_matched := NULL]
  }

  return(data)
}

## removes all pairs with less than n_required matched controls
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @importFrom data.table .N
remove_unmatched <- function(data, n_required) {

  .id_pair <- .count_id_pair <- NULL

  data <- copy(data)

  data[, .count_id_pair := .N, by=.id_pair]
  data <- data[.count_id_pair >= n_required + 1]
  data[, .count_id_pair := NULL]

  return(data)
}
