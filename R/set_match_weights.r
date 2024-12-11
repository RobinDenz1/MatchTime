
## adds matching weights to the dataset
## NOTE: uses the formula given by Noah Greifer
##       (see https://ngreifer.github.io/blog/matching-weights/)
## This is not a very general implementation (as done in MatchIt)
## but it allows weights / propensity score to be calculated for all
## methods directly implemented in this package.
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table setnames
set_match_weights <- function(data, treat, estimand, keep_ps=FALSE) {

  .treat <- .id_pair <- .ps_score <- .weights <- NULL

  setnames(data, old=treat, new=".treat")

  # calculate stratum propensity score
  data[, .ps_score := sum(.treat) / .N, by=.id_pair]

  # calculate weights, depending on estimand
  if (estimand=="ATT") {
    data[, .weights := fifelse(.treat==TRUE, 1, .ps_score / (1 - .ps_score))]
  } else if (estimand=="ATC") {
    data[, .weights := fifelse(.treat==TRUE, (1 - .ps_score)/.ps_score, 1)]
  } else {
    stop("Only 'ATT' and 'ATC' estimands are currently supported.")
  }

  if (!keep_ps) {
    data[, .ps_score := NULL]
  }
  setnames(data, old=".treat", new=treat)
}
