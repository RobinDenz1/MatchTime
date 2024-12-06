
## removes all cases that were not matched to >= n_required controls
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @export
match_data <- function(object, remove_unmatched=TRUE,
                       n_required=object$info$ratio) {

  .id_pair <- .count_id_pair <- .fully_matched <- NULL

  stopifnotm(inherits(object, "match_td"),
    "'object' must be a match_td object created using the match_td() function.")
  stopifnotm(is_single_logical(remove_unmatched),
             "'remove_unmatched' must be either TRUE or FALSE.")
  stopifnotm(length(n_required)==1 && is.numeric(n_required) &&
               n_required >= 1,
             "'n_required' should be a single integer.")

  data <- copy(object$data)

  if (remove_unmatched & n_required==object$info$ratio) {
    data <- data[.fully_matched==TRUE]
    data[, .fully_matched := NULL]
  } else if (remove_unmatched) {
    data[, .count_id_pair := .N, by=.id_pair]
    data <- data[.count_id_pair >= n_required + 1]
    data[, .count_id_pair := NULL]
    data[, .fully_matched := NULL]
  }

  return(data)
}
