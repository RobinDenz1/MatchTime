
## check whether an object is either TRUE or FALSE
is_single_logical <- function(x) {
  length(x)==1 && is.logical(x)
}

## check whether an object is a single character string
is_single_character <- function(x) {
  length(x)==1 && is.character(x)
}

## check whether a column is a Date or POSIXt object
is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt", "POSIXct", "POSIXlt"))
}

## works similar to stopifnot() but allows a custom message in
## a more convenient fashion
stopifnotm <- function(assert, message) {
  if (!assert) {
    stop(message, call.=FALSE)
  }
}

## input checks for the match_time() function
#' @importFrom fastmatch %fin%
check_inputs_match_time <- function(formula, data, id, inclusion,
                                  replace_over_t, replace_at_t,
                                  replace_cases, estimand, ratio,
                                  if_no_match, match_method, verbose,
                                  start, stop) {

  # correct data
  stopifnotm(is_single_character(start) && start %in% colnames(data),
             "'data' has to contain a column named 'start'.")
  stopifnotm(is_single_character(stop) && stop %in% colnames(data),
             "'data' has to contain a column named 'stop'.")
  stopifnotm(is.numeric(data[[start]]) || is.Date(data[[start]]),
             "'start' must contain integers, continuous numbers or dates.")
  stopifnotm(is.numeric(data[[stop]]) || is.Date(data[[stop]]),
             "'stop' must contain integers, continuous numbers or dates.")
  stopifnotm(!anyNA(data), "'data' may not contain missing values.")

  # correct formula
  stopifnotm(inherits(formula, "formula"),
             "'formula' must be a formula object.")

  not_in_data <- all.vars(formula)[!all.vars(formula) %fin% data]
  stopifnotm(all(all.vars(formula) %fin% colnames(data)),
             paste0("Variables ", paste0(not_in_data, collapse=", "),
                    " were not found in 'data'."))

  # correct treatment
  treat <- all.vars(formula)[1]
  stopifnotm(length(unique(data[[treat]]))==2,
             paste0("The treatment variable specified on the LHS of the",
                    " 'formula' argument must be binary."))

  # correct id
  stopifnotm(length(id)==1 && is.character(id) && id %fin% colnames(data),
             paste0("'id' must be a single character string specifying",
                    " a variable in 'data'."))

  # correct inclusion
  stopifnotm(is.na(inclusion) || (length(inclusion)==1 &&
              is.character(inclusion) &&
              inclusion %fin% colnames(data) &&
              is.logical(data[[inclusion]])),
             paste0("'inclusion' must be a single character string specifying",
                    " a logical variable in 'data' or NA."))

  # correct ratio
  stopifnotm(length(ratio)==1 && is.numeric(ratio) && ratio >= 1 &&
               round(ratio)==ratio,
             "'ratio' must be a single integer >= 1.")

  # correct if_no_match
  stopifnotm(is_single_character(if_no_match) &&
              if_no_match %fin% c("stop", "warn", "nothing"),
             "'if_no_match' must be either 'stop', 'warn' or 'nothing'.")

  # correct match_method
  stopifnotm(is_single_character(match_method) &&
               match_method %in% c("none", "fast_exact", "nearest",
                                   "optimal", "full", "genetic", "cem",
                                   "exact", "cardinality", "subclass"),
             paste0("'match_method' must be either 'none', 'fast_exact' or ",
                    " a valid 'method' in MatchIt::matchit()."))

  # correct logical variables
  stopifnotm(is_single_logical(replace_over_t),
             "'replace_over_t' must be either TRUE or FALSE.")
  stopifnotm(is_single_logical(replace_at_t),
             "'replace_at_t' must be either TRUE or FALSE.")
  stopifnotm(is_single_logical(replace_cases),
             "'replace_cases' must be either TRUE or FALSE.")
  stopifnotm(is_single_logical(verbose),
             "'verbose' must be either TRUE or FALSE.")

  # correct estimand
  stopifnotm(is_single_character(estimand) && estimand %in% c("ATT", "ATC"),
             paste0("'estimand' must be either 'ATT' or 'ATC'."))
}

## check inputs for the fast_exact_matching() function
#' @importFrom fastmatch %fin%
check_inputs_fast_exact_matching <- function(data, treat, strata, replace,
                                             ratio, estimand, if_no_match) {

  # correct treat
  stopifnotm(is_single_character(treat) && length(unique(data[[treat]]))==2,
             paste0("The treatment variable specified on the LHS of the",
                    " 'formula' argument must specify a binary variable",
                    " in 'data'."))

  # correct strata
  stopifnotm(is_single_character(strata) && strata %in% colnames(data),
             paste0("'strata' must be a single character string specifying",
                    " a binary or categorical variable in 'data'."))

  # correct ratio
  stopifnotm(length(ratio)==1 && is.numeric(ratio) && ratio >= 1 &&
               round(ratio)==ratio,
             "'ratio' must be a single integer >= 1.")

  # correct if_no_match
  stopifnotm(is_single_character(if_no_match) &&
              if_no_match %fin% c("stop", "warn", "nothing"),
             "'if_no_match' must be either 'stop', 'warn' or 'nothing'.")

  # correct estimand
  stopifnotm(is_single_character(estimand) && estimand %fin% c("ATT", "ATC"),
             "'estimand' must be either 'ATT' or 'ATC'.")

  # correct replace
  stopifnotm(is_single_logical(replace),
             "'replace' must be either TRUE or FALSE.")
}

## input checks for multiple add_...() functions
check_inputs_add_variable <- function(x, id, time, data, name, include_same_t) {

  stopifnotm(inherits(x, "match_time"),
             paste0("'x' must be a 'match_time' object created using the",
                    " match_time() function."))
  stopifnotm(is_single_logical(include_same_t),
             "'include_same_t' must be either TRUE or FALSE.")
  stopifnotm(is_single_character(name),
             "'name' must be a single character string.")
  stopifnotm(is_single_character(id),
             "'id' must be a single character string.")
  stopifnotm(is_single_character(time),
             "'time' must be a single character string.")

  # data
  stopifnotm(ncol(data)==2,
             paste0("'data' should only have two columns, containing the ",
                    "case id and the time."))
  stopifnotm(id %in% colnames(data),
             "'id' must specify a valid column in 'data'.")
  stopifnotm(time %in% colnames(data),
             "'time' must specify a valid column in 'data'.")
}
