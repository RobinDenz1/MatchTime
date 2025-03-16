
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
stopifnotm <- function(assert, ...) {
  if (!assert) {
    stop(paste(..., collapse=""), call.=FALSE)
  }
}

## similar to stopifnotm() but just returns a warning
warnifnotm <- function(assert, ...) {
  if (!assert) {
    warning(paste(..., collapse=""), call.=FALSE)
  }
}

## extract types of columns from data
col_types <- function(data, cols) {
  lapply(cols, FUN=function(data, x){class(data[[x]])}, data=data)
}

## input checks for the match_time() function
#' @importFrom fastmatch %fin%
check_inputs_match_time <- function(formula, data, id, inclusion,
                                    replace_over_t, replace_at_t,
                                    replace_cases, estimand, ratio,
                                    recruitment_start, recruitment_stop,
                                    match_method, verbose,
                                    start, stop, method, outcomes) {

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
  form <- process_formula(formula)
  stopifnotm(length(unique(data[[form$treat]]))==2,
             paste0("The treatment variable specified on the LHS of the",
                    " 'formula' argument must be binary."))

  # correct id
  stopifnotm(length(id)==1 && is.character(id) && id %fin% colnames(data),
             paste0("'id' must be a single character string specifying",
                    " a variable in 'data'."))

  # correct inclusion
  stopifnotm(all(is.na(inclusion)) || (length(inclusion) >= 1 &&
              is.character(inclusion) &&
              all(inclusion %fin% colnames(data)) &&
              all(unlist(col_types(data=data, cols=inclusion))=="logical")),
             paste0("'inclusion' must be a character vector specifying",
                    " logical variables in 'data' or NA."))

  # correct outcomes
  stopifnotm(all(is.na(outcomes)) || (is.character(outcomes) &&
               all(outcomes %in% colnames(data))),
             "'outcomes' must be a character vector specifying logical",
             "event variables in 'data'.")
  stopifnotm(all(is.na(outcomes)) ||
             all(unlist(lapply(data[, outcomes, with=FALSE], is.logical))),
             "All columns named in 'outcomes' must be logical.")
  form_in_outcomes <- form$match_vars[form$match_vars %in% outcomes]
  stopifnotm(length(form_in_outcomes)==0,
             "Cannot match on variables specified as outcome(s):",
             paste0(form_in_outcomes, collapse=", "))

  # correct recruitment_start
  stopifnotm(is.null(recruitment_start) || (length(recruitment_start==1)),
             "'recruitment_start' must be a single number or Date or NULL.")

  # correct recruitment_stop
  stopifnotm(is.null(recruitment_stop) || (length(recruitment_stop==1)),
             "'recruitment_stop' must be a single number or Date or NULL.")

  # correct ratio
  stopifnotm(length(ratio)==1 && is.numeric(ratio) && ratio >= 1 &&
               round(ratio)==ratio,
             "'ratio' must be a single integer >= 1.")

  # correct match_method
  stopifnotm(is_single_character(match_method) &&
               match_method %in% c("none", "fast_exact", "nearest",
                                   "optimal", "full", "genetic", "cem",
                                   "exact", "cardinality", "subclass"),
             paste0("'match_method' must be either 'none', 'fast_exact' or ",
                    " a valid 'method' in MatchIt::matchit()."))

  # correct method
  stopifnotm(is.character(method[1]) &&
               method[1] %in% c("brsm", "psm", "pgm", "dsm", "greedy"),
             "'method' must be either 'brsm', 'psm', 'pgm', 'dsm' or 'greedy'.")

  # with method="psm"
  stopifnotm(!(method[1]=="psm" && match_method=="fast_exact"),
             "Cannot use match_method='fast_exact' with method='psm'.")
  stopifnotm(!(method[1]=="psm" && length(all.vars(formula))==1),
             paste0("'formula' must contain at least one variable on the RHS",
                    " if method='psm'."))

  # with method="pgm"
  stopifnotm(!(method[1]=="pgm" && match_method=="fast_exact"),
             "Cannot use match_method='fast_exact' with method='pgm'.")
  stopifnotm(!(method[1]=="pgm" && length(all.vars(formula))==1),
             paste0("'formula' must contain at least one variable on the RHS",
                    " if method='pgm'."))

  # with method="dsm"
  stopifnotm(!(method[1]=="dsm" && match_method=="fast_exact"),
             "Cannot use match_method='fast_exact' with method='dsm'.")
  stopifnotm(!(method[1]=="dsm" && length(all.vars(formula))==1),
             paste0("'formula' must contain at least one variable on the RHS",
                    " if method='dsm'."))

  # with method greedy
  warnifnotm(method[1]!= "greedy" |
               (method[1]=="greedy" && length(form$match_vars) == 0),
             "Variables on the RHS of 'formula' are ignored",
             "because method='greedy' is used.")

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
  stopifnotm(is_single_character(estimand) && estimand=="ATT",
             paste0("'estimand' must be 'ATT'."))
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
