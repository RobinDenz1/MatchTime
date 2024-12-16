
## adds matching weights to the dataset
## NOTE: uses the formula given by Noah Greifer
##       (see https://ngreifer.github.io/blog/matching-weights/)
## This is not a very general implementation (as done in MatchIt)
## but it allows weights / propensity score to be calculated for all
## methods directly implemented in this package.
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table setnames
set_match_weights <- function(data, treat, estimand, keep_ps=FALSE,
                              stabilize=TRUE) {

  .treat <- .id_pair <- .ps_score <- .weights <- .mean_w <- NULL

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

  # stabilize the weights to sum to the number of observations in each group
  if (stabilize) {
    mean_cases <- mean(data[.weights > 0 & .treat==TRUE]$.weights)
    mean_controls <- mean(data[.weights > 0 & .treat==FALSE]$.weights)
    data[, .mean_w := fifelse(.treat==TRUE, mean_cases, mean_controls)]
    data[, .weights := .weights / .mean_w]
    data[, .mean_w := NULL]
  }

  if (!keep_ps) {
    data[, .ps_score := NULL]
  }
  setnames(data, old=".treat", new=treat)
}
