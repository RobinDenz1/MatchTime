
## removes all cases that were not matched to >= n_required controls
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @export
remove_unmatched <- function(x, n_required=x$info$ratio) {

  .id_pair <- .count_id_pair <- NULL

  stopifnotm(inherits(x, "match_td"),
         "'x' must be a match_td object created using the match_td() function.")
  stopifnotm(length(n_required)==1 && is.numeric(n_required) &&
               n_required >= 1,
             "'n_required' should be a single integer.")

  x <- copy(x)

  x$data[, .count_id_pair := .N, by=.id_pair]
  x$data <- x$data[.count_id_pair >= n_required + 1]
  x$data[, .count_id_pair := NULL]

  return(x)
}
