
## re-code integers, factors or characters to TRUE / FALSE treatment
# NOTE: assumes that it has already been checked that treat only contains
#       two values
#' @importFrom data.table fifelse
preprocess_treat <- function(treat) {

  if (is.numeric(treat) && all(treat %in% c(0, 1))) {
    treat <- fifelse(treat==0, FALSE, TRUE)
  } else if (is.factor(treat)) {
    treat <- fifelse(treat==levels(treat)[1], FALSE, TRUE)
  } else if (is.character(treat)) {
    treat <- fifelse(treat==sort(unique(treat))[1], FALSE, TRUE)
  } else {
    stop("The treatment variable specified by the LHS of 'formula'",
         " needs to specify a variable coded as one of:\n ",
         "(1) a logical vector, (2) an integer with only 0/1 values",
         ", (3) a binary factor or (4) a binary character variable.")
  }
  return(treat)
}

## fit a cox model to predict the treatment probability at t, which is
## the time-dependent propensity score as defined in Lu (2005)
fit_ps_model <- function(data, d_treat, match_vars, formula) {

  requireNamespace("survival")

  setnames(d_treat, old=".time", new="time")

  # create a new start-stop dataset with A as terminal outcome event
  d_ps_mod <- merge_start_stop(data, event_times=d_treat, by=".id",
                               all=TRUE, time_to_first_event=TRUE,
                               status=".treat", start=".start",
                               stop=".stop")
  setnames(d_treat, old="time", new=".time")

  # fit cox model
  if (!is.null(formula)) {
    formula <- paste(deparse(formula), collapse=" ")
    cox_form <- paste0("survival::Surv(.start, .stop, .treat) ", formula)
  } else {
    cox_form <- paste0("survival::Surv(.start, .stop, .treat) ~ ",
                       paste0(match_vars, collapse=" + "))
  }

  ps_model <- survival::coxph(formula=stats::as.formula(cox_form),
                              data=d_ps_mod)

  return(ps_model)
}

## fit an outcome cox model to predict the prognostic score for matching
#' @importFrom data.table merge.data.table
fit_prog_model <- function(data, d_treat, event, match_vars, formula) {

  requireNamespace("survival")

  # create a new start-stop dataset with everything after treatment removed
  d_outcome <- merge.data.table(data, d_treat, by=".id", all.x=TRUE)
  d_outcome <- subset_start_stop(d_outcome, last_time=d_outcome$.time,
                                 na.rm=FALSE, start=".start", stop=".stop")

  # fit cox model
  if (!is.null(formula)) {
    formula <- paste(deparse(formula), collapse=" ")
    cox_form <- paste0("survival::Surv(.start, .stop, ", event, ") ", formula)
  } else {
    cox_form <- paste0("survival::Surv(.start, .stop, ", event, ") ~ ",
                       paste0(match_vars, collapse=" + "))
  }

  prog_model <- survival::coxph(formula=stats::as.formula(cox_form),
                                data=d_outcome)

  return(prog_model)
}

## transforms a numeric vector to the 0 / 1 range
scale_0_1 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

## removing some cols that are used internally with different methods,
## if specified
#' @importFrom data.table :=
set_remove_cols <- function(data, method, ps_type, prog_type) {

  .ps_score <- .lp_ps <- .prog_score <- .lp_prog <- NULL

  # method = "psm"
  if (method[1]=="psm") {
    data[, .ps_score := NULL]

    if (ps_type[1]=="ps") {
      data[, .lp_ps := NULL]
    }
  }

  # method = "pgm"
  if (method[1]=="pgm") {
    data[, .prog_score := NULL]

    if (prog_type[1]=="p") {
      data[, .lp_prog := NULL]
    }
  }
}

## add time-dependent propensity score or the
## time-dependent prognostic score to data
#' @importFrom data.table :=
set_score_at_t <- function(data, h0, t, name_score, name_lp,
                           standardize) {
  data[, (name_score) := h0(t) * exp(get(name_lp))]

  if (standardize) {
    data[, (name_score) := scale_0_1(get(name_score))]
  }
}
