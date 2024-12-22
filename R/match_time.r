
## main function of the package, but is only a wrapper around the true
## estimation function called match_time.fit()
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @export
match_time <- function(formula, data, id, inclusion=NA,
                       start="start", stop="stop",
                       replace_over_t=FALSE, replace_at_t=FALSE,
                       replace_cases=TRUE, estimand="ATT", ratio=1,
                       match_method="fast_exact", if_no_match="warn",
                       verbose=FALSE, save_matchit=FALSE, ...) {

  .inclusion <- .treat <- .time <- NULL

  # coerce to data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  # check inputs
  check_inputs_match_time(formula=formula, data=data, id=id,
                          inclusion=inclusion,
                          replace_over_t=replace_over_t,
                          replace_at_t=replace_at_t,
                          replace_cases=replace_cases,
                          estimand=estimand, ratio=ratio,
                          if_no_match=if_no_match,
                          match_method=match_method,
                          verbose=verbose,
                          start=start, stop=stop)

  # extract needed things from formula
  vars <- all.vars(formula)
  treat <- vars[1]
  if (length(vars) > 1) {
    match_vars <- vars[2:length(vars)]
  } else {
    match_vars <- NULL
  }

  # fix treatment variable if needed
  if (!is.logical(data[[treat]])) {
    setnames(data, old=treat, new=".treat")
    data[, .treat := preprocess_treat(.treat)]
    setnames(data, old=".treat", new=treat)
  }

  # extract relevant treatment times
  d_treat <- times_from_start_stop(data=data, name=treat, id=id,
                                   type="var", time_name=".time",
                                   start=start, stop=stop)
  data[, (treat) := NULL]

  # save sample sizes of input
  n_input_all <- length(unique(data[[id]]))
  n_input_cases <- nrow(d_treat)
  n_input_controls <- n_input_all - nrow(d_treat[.time==0])

  # remove all rows when inclusion criteria are not met
  if (!is.na(inclusion)) {
    setnames(data, old=inclusion, new=".inclusion")
    data <- data[.inclusion==TRUE]
    data[, .inclusion := NULL]
  }

  # call function that does all the work
  out <- match_time.fit(id=id, time=".time", d_treat=d_treat,
                        d_covars=data, match_vars=match_vars,
                        replace_over_t=replace_over_t,
                        replace_at_t=replace_at_t, replace_cases=replace_cases,
                        estimand=estimand, ratio=ratio, if_no_match=if_no_match,
                        match_method=match_method, verbose=verbose,
                        start=start, stop=stop, save_matchit=save_matchit)

  # add stuff to output
  out$call <- match.call()
  out$sizes$n_input_all <- n_input_all
  out$sizes$n_input_cases <- n_input_cases
  out$sizes$n_input_controls <- n_input_controls

  return(out)
}

## perform time-dependent matching
#' @importFrom fastmatch %fin%
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table copy
#' @importFrom data.table .GRP
#' @importFrom data.table rbindlist
#' @importFrom data.table setcolorder
match_time.fit <- function(id, time, d_treat, d_covars,
                           match_vars=NULL, replace_over_t=FALSE,
                           replace_at_t=FALSE, replace_cases=TRUE,
                           estimand="ATT", ratio=1,
                           if_no_match="warn", match_method="fast_exact",
                           verbose=FALSE, start="start", stop="stop",
                           save_matchit=FALSE, ...) {

  .treat <- .id_pair <- .subclass <- .treat_time <- .strata <- .start <-
    .id_new <- .next_treat_time <- .time <- .stop <- .id <-
    .fully_matched <- .weights <- ..id.. <- .distance <- NULL

  # rename id / time to prevent possible errors with get()
  setnames(d_treat, old=c(id, time), new=c(".id", ".time"))
  setnames(d_covars, old=c(id, start, stop),
           new=c(".id", ".start", ".stop"))

  # get variables that should be matched on
  if (is.null(match_vars)) {
    cnames <- colnames(d_covars)
    match_vars <- cnames[!cnames %fin% c(".id", ".time", ".treat", ".start",
                                         ".stop")]
  }
  cnames <- colnames(d_covars)
  select_vars <- cnames[!cnames %fin% c(".start", ".stop")]

  if (match_method %in% c("none", "fast_exact")) {
    select_vars <- c(select_vars, ".strata")
  }

  # get maximum follow-up time per person (used for adding events later)
  d_longest <- d_covars[, (.max_t = max(.stop)), by=".id"]
  colnames(d_longest) <- c(id, ".max_t")

  # keep only cases that meet inclusion criteria at treatment time
  d_covars <- merge.data.table(d_covars, d_treat, by=".id", all.x=TRUE)
  include <- d_covars[.time >= .start & .time < .stop]$.id
  d_treat <- d_treat[.id %fin% include]

  # save sample sizes after applying inclusion criteria
  n_incl_all <- length(unique(d_covars$.id))
  n_incl_cases <- nrow(d_treat)
  n_incl_controls <- n_incl_all - nrow(d_treat[.time==0])

  # identify all points in time at which at least one case happened
  case_times <- sort(unique(d_treat$.time))

  # remove time durations after treatment onset
  min_t_passed <- as.numeric(
    min(shift(case_times, type="lead") - case_times, na.rm=TRUE)
  )
  d_covars <- subset_start_stop(data=d_covars,
                                last_time=d_covars$.time + min_t_passed,
                                start=".start", stop=".stop")
  d_covars[, .time := NULL]

  # set up .strata for fast_exact_matching(), if used
  if (match_method=="none") {
    d_covars[, .strata := TRUE]
  } else {
    d_covars[, .strata := do.call(paste0, .SD), .SDcols=match_vars]
  }

  # initialize needed id collections
  used_as_controls <- c()
  used_as_cases <- c()

  out <- trace <- matchit_out <- vector(mode="list", length=length(case_times))
  for (i in seq_len(length(case_times))) {

    # identify new cases at t
    ids_cases_i <- d_treat[.time==case_times[i]]$.id

    # potentially remove cases that were previously used as controls
    if (!replace_cases) {
      ids_cases_i <- ids_cases_i[!ids_cases_i %fin% used_as_controls]
    }

    # skip this time point if there are no cases to be matched
    # (this may only happen when setting replace_cases=FALSE)
    if (length(ids_cases_i)==0) {
      next
    }

    # update collection of ids that were used as cases
    used_as_cases <- c(used_as_cases, ids_cases_i)

    # identify all potential controls that fulfill inclusion criteria at t
    ids_pot_controls_i <- unique(d_covars$.id)
    ids_pot_controls_i <- ids_pot_controls_i[!ids_pot_controls_i %fin%
                                               used_as_cases]

    # potentially remove controls that were already used as controls
    if (!replace_over_t) {
      ids_pot_controls_i <- ids_pot_controls_i[!ids_pot_controls_i %fin%
                                                used_as_controls]
    }

    # pool all relevant ids
    all_ids <- c(ids_pot_controls_i, ids_cases_i)

    # get dataset including them all at t
    d_all_i <- d_covars[.id %fin% all_ids &
                        case_times[i] >= .start & case_times[i] < .stop
                        ][, select_vars, with=FALSE]

    # identify cases
    d_all_i[, .treat := .id %fin% ids_cases_i]

    # return only cases if no controls left
    if (nrow(d_all_i)==length(ids_cases_i)) {

      d_match_i <- d_all_i
      d_match_i[, .treat_time := case_times[i]]
      d_match_i[, .weights := 1]

      if (match_method %in% c("none", "fast_exact")) {
        d_match_i[, .strata := NULL]
      }

      if (match_method %in% c("none", "fast_exact", "nearest", "full",
                              "genetic")) {
        d_match_i[, .id_pair := paste0(i, "_", seq_len(.N))]
      }

    # fast exact or no matching
    } else if (match_method=="fast_exact" || match_method=="none") {

      # perform exact matching
      d_match_i <- fast_exact_matching.fit(d_all_i,
                                           treat=".treat",
                                           strata=".strata",
                                           estimand="ATT",
                                           replace=replace_at_t,
                                           if_no_match=if_no_match,
                                           ratio=ratio)

      d_match_i[, .id_pair := paste0(i, "_", .id_pair)]
      d_match_i[, .treat_time := case_times[i]]
      d_match_i[, .strata := NULL]

    } else {

      requireNamespace("MatchIt")

      # create formulas for matchit call
      main_formula <- paste0(".treat ~ ", paste0(match_vars, collapse=" + "))

      # perform matching on baseline covariates
      args <- list(formula=stats::as.formula(main_formula),
                   data=d_all_i,
                   method=match_method,
                   estimand="ATT",
                   replace=replace_at_t,
                   ratio=ratio)
      args <- c(args, list(...))

      d_match_i <- do.call(MatchIt::matchit, args=args)

      if (save_matchit) {
        matchit_out[[i]] <- d_match_i
      }

      # extract matched data, conditional on method used
      if (!match_method %in% c("nearest", "full", "genetic")) {
        d_match_i <- MatchIt::match.data(d_match_i,
                                         weights=".weights",
                                         subclass=".subclass",
                                         distance=".distance")
      } else {
        d_match_i <- MatchIt::get_matches(d_match_i,
                                          weights=".weights",
                                          subclass=".subclass",
                                          distance=".distance",
                                          id="..id..")
      }
      d_match_i <- as.data.table(d_match_i)
      d_match_i[, ..id.. := NULL]

      # clean up data
      d_match_i <- copy(d_match_i)

      if (match_method %in% c("nearest", "full", "genetic")) {
        d_match_i[, .id_pair := paste0(i, "_", .subclass)]
      }
      if (".distance" %in% colnames(d_match_i)) {
        d_match_i[, .distance := NULL]
      }
      if (".subclass" %in% colnames(d_match_i)) {
        d_match_i[, .subclass := NULL]
      }

      # add matched time
      d_match_i[, .treat_time := case_times[i]]
    }

    # update used_as_controls vector
    controls_i <- unique(d_match_i[.treat==FALSE]$.id)
    used_as_controls <- c(used_as_controls, controls_i)

    # append to output
    out[[i]] <- d_match_i

    # save the trace of the functions work
    n_m_controls_i <- length(controls_i)
    n_cases_i <- length(ids_cases_i)
    n_pot_controls_i <- nrow(d_all_i) - n_cases_i
    trace[[i]] <- data.table(time=case_times[i],
                             new_cases=n_cases_i,
                             matched_controls=n_m_controls_i,
                             potential_controls=n_pot_controls_i)

    if (verbose) {
      cat("Matched ", n_m_controls_i, " controls to ",
          n_cases_i, " cases (with ", n_pot_controls_i,
          " potential unique controls) at t = ", as.character(case_times[i]),
          ".\n", sep="")
    }
  }

  # full dataset
  data <- rbindlist(out, use.names=TRUE)
  rm(out)

  # create new .id_new to differentiate between persons
  data[, .id_new := .I]

  # clean up .id_pair, if it exists
  if (".id_pair" %in% colnames(data)) {
    data[, .id_pair := .GRP, by=".id_pair"]
  }

  # for controls, add time of next treatment
  setnames(d_treat, old=".time", new=".next_treat_time")
  data <- merge.data.table(data, d_treat, by=".id", all.x=TRUE)
  data[.treat==TRUE, .next_treat_time := NA]

  # calculate number of actually matched individuals
  if (".id_pair" %in% colnames(data)) {
    data[, .fully_matched := .N == ratio + 1, by=.id_pair]
  } else {
    data[, .fully_matched := TRUE]
  }

  n_matched_cases <- nrow(data[.fully_matched==TRUE & .treat==TRUE])
  n_matched_controls <- nrow(data[.fully_matched==TRUE & .treat==FALSE])

  # change order of columns
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

  # put together output
  out <- list(data=data,
              d_longest=d_longest,
              id=id,
              time=time,
              info=list(replace_over_t=replace_over_t,
                        replace_at_t=replace_at_t,
                        replace_cases=replace_cases,
                        estimand=estimand,
                        ratio=ratio,
                        match_method=match_method,
                        match_vars=match_vars,
                        added_event_times=c(),
                        added_status=c(),
                        added_next_time=c()),
              sizes=list(n_unmatched_controls=sum(!unique(d_covars[[id]]) %in%
                                                  data[[id]]),
                         n_unmatched_cases=nrow(data) - (n_matched_cases +
                           n_matched_controls),
                         n_matched_cases=n_matched_cases,
                         n_matched_controls=n_matched_controls,
                         n_incl_all=n_incl_all,
                         n_incl_cases=n_incl_cases,
                         n_incl_controls=n_incl_controls),
              trace=rbindlist(trace),
              matchit_objects=NULL)
  class(out) <- "match_time"

  if (save_matchit) {
    names(matchit_out) <- as.character(case_times)
    out$matchit_objects <- matchit_out
  }

  return(out)
}

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
