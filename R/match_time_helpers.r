
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
set_remove_cols <- function(data, method, ps_type, prog_type,
                            remove_ps, remove_prog) {

  .ps_score <- .lp_ps <- .prog_score <- .lp_prog <- NULL

  # method = "psm"
  if ((method[1]=="psm" | method[1]=="dsm") & remove_ps) {
    data[, .ps_score := NULL]
  }
  if ((method[1]=="psm" | method[1]=="dsm") & ps_type[1]=="ps") {
    data[, .lp_ps := NULL]
  }

  # method = "pgm"
  if ((method[1]=="pgm" | method[1]=="dsm") & remove_prog) {
    data[, .prog_score := NULL]
  }
  if ((method[1]=="pgm" | method[1]=="dsm") & prog_type[1]=="p") {
    data[, .lp_prog := NULL]
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

## extracts treatment and variables from formula
process_formula <- function(formula) {

  match_vars <- all.vars(formula[-2])
  treat <- all.vars(formula)[!all.vars(formula) %in% match_vars]

  stopifnotm(is_single_character(treat),
          "There must be a single variable on the left hand side of 'formula'.")

  if (length(match_vars)==0) {
    match_vars <- NULL
  }

  out <- list(treat=treat, match_vars=match_vars)
  return(out)
}

## adding further columns to be selected in matching process,
## based on user-supplied arguments
update_select_vars <- function(select_vars, method, ps_type, prog_type) {

  # add propensity score variables to vector of names that will be included
  if ((method=="psm" | method=="dsm") & ps_type[1]=="ps") {
    select_vars <- c(select_vars, ".lp_ps")
  } else if (method=="psm" | method=="dsm") {
    select_vars <- c(select_vars, ".ps_score")
  }

  # add prognostic score variables to vector of names that will be included
  if ((method=="pgm" | method=="dsm") & prog_type[1]=="p") {
    select_vars <- c(select_vars, ".lp_prog")
  } else if (method=="pgm" | method=="dsm") {
    select_vars <- c(select_vars, ".prog_score")
  }

  return(select_vars)
}

## remove time durations where inclusion criteria are not fulfilled,
## and return a list of reasons for exclusion in 2 stages
apply_inclusion_criteria <- function(d_covars, inclusion, method) {

  .id <- .inclusion <- .remove_all <- .start <- .stop <-
    .time <- .treat <- .treat_at_0 <- NULL

  .incl <- inclusion
  d_covars[, .inclusion := rowSums(.SD) == length(.incl), .SDcols=.incl]

  if (method != "dynamic") {
    # never meet inclusion criteria
    d_covars[, .remove_all := sum(!.inclusion)==.N, by=.id]
    d_exclusion1 <- d_covars[.remove_all==TRUE][, c(".id", .incl, ".time"),
                                                with=FALSE]
    d_exclusion1[, .treat := !is.na(.time)]
    d_exclusion1[, .treat_at_0 := fifelse(.time==0, TRUE, FALSE, na=FALSE)]
    d_exclusion1 <- d_exclusion1[, lapply(.SD, all), by=".id",
                                 .SDcols=c(.incl, ".treat", ".treat_at_0")]

    # inclusion criteria not met at treatment time
    d_exclusion2 <- d_covars[.time >= .start & .time < .stop & !.inclusion &
                               !.remove_all]
    d_exclusion2 <- d_exclusion2[, c(".id", .incl), with=FALSE]

    d_covars[, .remove_all := NULL]
  } else {
    d_exclusion1 <- NULL
    d_exclusion2 <- NULL
  }

  # remove all rows when inclusion criteria are not met
  d_covars <- d_covars[.inclusion==TRUE]
  d_covars[, c(inclusion, ".inclusion") := NULL]

  if (nrow(d_covars)==0) {
    stop("There are no observations left after applying the",
         " inclusion criteria.", call.=FALSE)
  }

  out <- list(d_covars=d_covars,
              stage1=d_exclusion1,
              stage2=d_exclusion2)
  return(out)
}

## fixes column order and names for output of match_time()
set_cols_matchdata <- function(data, d_covars, id) {

  if (".id_pair" %in% colnames(data)) {
    first_cols <- c(".id", ".id_new", ".id_pair", ".treat", ".treat_time",
                    ".next_treat_time", ".fully_matched", ".weights")
  } else {
    first_cols <- c(".id", ".id_new", ".treat", ".treat_time",
                    ".next_treat_time", ".fully_matched", ".weights")
  }

  last_cols <- colnames(data)[!colnames(data) %fin% first_cols]
  setcolorder(data, c(first_cols, last_cols))
  setnames(data, old=".id", new=id)
  setnames(d_covars, old=".id", new=id)
}

## define main formula for matching
get_main_formula <- function(method, match_vars) {

  if (method=="brsm") {
    main_formula <- paste0(".treat ~ ", paste0(match_vars, collapse=" + "))
  } else if (method=="psm") {
    main_formula <- ".treat ~ .ps_score"
  } else if (method=="pgm") {
    main_formula <- ".treat ~ .prog_score"
  } else if (method=="dsm") {
    main_formula <- ".treat ~ .ps_score + .prog_score"
  }

  return(main_formula)
}

## adds propensity score to the data, also returns baseline hazard
## function if needed
set_propensity_score <- function(d_covars, ps_model, ps_type,
                                 standardize_ps, basehaz_interpol) {

  .ps_score <- .lp_ps <- NULL

  # directly use linear predictor as propensity score, as done in
  # Hade et al. (2020)
  if (ps_type[1]=="lp") {
    d_covars[, .ps_score := stats::predict(ps_model, newdata=d_covars)]
    if (standardize_ps) {
      d_covars[, .ps_score := scale_0_1(.ps_score)]
    }
    h0_ps <- NULL
  # or use actual propensity score as done in Lu (2005)
  } else if (ps_type[1]=="ps") {
    # calculate linear predictor & estimate baseline hazard
    d_covars[, .lp_ps := stats::predict(ps_model, newdata=d_covars)]
    h0_ps <- survival::basehaz(ps_model)
    h0_ps <- stats::approxfun(x=h0_ps$time, y=h0_ps$hazard,
                              method=basehaz_interpol, rule=2)
  }

  return(h0_ps)
}

## adds prognostic score to the data, also returns baseline hazard
## function if needed
set_prognostic_score <- function(d_covars, prog_model, prog_type,
                                 standardize_prog, basehaz_interpol) {

  .prog_score <- .lp_prog <- NULL

  # directly use linear predictor as prognostic score
  if (prog_type[1]=="lp") {
    d_covars[, .prog_score := stats::predict(prog_model, newdata=d_covars)]
    if (standardize_prog) {
      d_covars[, .prog_score := scale_0_1(.prog_score)]
    }
    h0_prog <- NULL
  # or use actual probability
  } else if (prog_type[1]=="p") {
    # calculate linear predictor & estimate baseline hazard
    d_covars[, .lp_prog := stats::predict(prog_model, newdata=d_covars)]
    h0_prog <- survival::basehaz(prog_model)
    h0_prog <- stats::approxfun(x=h0_prog$time, y=h0_prog$hazard,
                                method=basehaz_interpol, rule=2)
  }

  return(h0_prog)
}

## checks if the treatment is valid
check_treatment <- function(data, id, method) {

  . <- NULL

  if (method != "dynamic") {
    d_count <- data[, .(n = .N), by=id]
    ids_multi_treat <- d_count[[id]][d_count$n > 1]

    stopifnotm(length(ids_multi_treat)==0,
               "There should only be one treatment time per person. Multiple",
               "treatments were detected for ", id, " = ", ids_multi_treat[1])
  }
}
