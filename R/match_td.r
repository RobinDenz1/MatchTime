
## main function of the package, but is only a wrapper around the true
## estimation function called match_td.fit()
#' @importFrom data.table is.data.table
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @export
match_td <- function(formula, data, id, inclusion=NA,
                     start="start", stop="stop",
                     replace_over_t=FALSE, replace_at_t=FALSE,
                     replace_cases=TRUE, estimand="ATT", ratio=1,
                     match_method="fast_exact", if_no_match="warn",
                     verbose=FALSE, ...) {

  .inclusion <- .treat <- NULL

  # coerce to data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  # check inputs
  check_inputs_match_td(formula=formula, data=data, id=id,
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

  # remove all rows when inclusion criteria are not met
  if (!is.na(inclusion)) {
    setnames(data, old=inclusion, new=".inclusion")
    data <- data[.inclusion==TRUE]
    data[, .inclusion := NULL]
  }

  # call function that does all the work
  out <- match_td.fit(id=id, time=".time", d_treat=d_treat,
                      d_covars=data, match_vars=match_vars,
                      replace_over_t=replace_over_t,
                      replace_at_t=replace_at_t, replace_cases=replace_cases,
                      estimand=estimand, ratio=ratio, if_no_match=if_no_match,
                      match_method=match_method, verbose=verbose,
                      start=start, stop=stop)

  # add call to output
  out$call <- match.call()

  return(out)
}

## perform time-dependent matching
#' @importFrom fastmatch %fin%
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table copy
#' @importFrom data.table rbindlist
#' @importFrom data.table setcolorder
match_td.fit <- function(id, time, d_treat, d_covars,
                         match_vars=NULL, replace_over_t=FALSE,
                         replace_at_t=FALSE, replace_cases=TRUE,
                         estimand="ATT", ratio=1,
                         if_no_match="warn", match_method="fast_exact",
                         verbose=FALSE, start="start", stop="stop",
                         ...) {

  .treat <- .id_pair <- subclass <- .treat_time <- .strata <- .start <-
    .id_new <- .next_treat_time <- .next_event_time <- .time <-
    .stop <- .id <- NULL

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

  # get maximum follow-up time per person (used for adding events later)
  d_longest <- d_covars[, (.max_t = max(.stop)), by=".id"]
  colnames(d_longest) <- c(id, ".max_t")

  # keep only cases that meet inclusion criteria at treatment time
  d_covars <- merge.data.table(d_covars, d_treat, by=".id", all.x=TRUE)
  include <- d_covars[.time >= .start & .time < .stop]$.id
  d_treat <- d_treat[.id %fin% include]

  # identify all points in time at which at least one case happened
  case_times <- sort(unique(d_treat$.time))

  # remove time durations after treatment onset
  min_t_passed <- min(shift(case_times, type="lead") - case_times, na.rm=TRUE)
  d_covars <- subset_start_stop(data=d_covars,
                                last_time=d_covars$.time + min_t_passed,
                                start=".start", stop=".stop")
  d_covars[, .time := NULL]

  # initialize needed id collections
  used_as_controls <- c()
  used_as_cases <- c()

  out <- trace <- vector(mode="list", length=length(case_times))
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
      d_match_i[, .id_pair := paste0(i, "_", seq_len(.N))]
      d_match_i[, .treat_time := case_times[i]]

    # fast exact or no matching
    } else if (match_method=="fast_exact" || match_method=="none") {

      # create strata variable
      if (match_method=="none") {
        d_all_i[, .strata := TRUE]
      } else {
        d_all_i[, .strata := do.call(paste0, .SD), .SDcols=match_vars]
      }

      # perform exact matching
      d_match_i <- fast_exact_matching.fit(d_all_i,
                                           treat=".treat",
                                           strata=".strata",
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
                   estimand=estimand,
                   replace=replace_at_t,
                   ratio=ratio)
      args <- c(args, list(...))

      d_match_i <- do.call(MatchIt::matchit, args=args)
      d_match_i <- MatchIt::match.data(d_match_i)

      # assign .id_pair
      d_match_i <- copy(d_match_i)
      d_match_i[, .id_pair := paste0(i, "_", subclass)]
      d_match_i[, .treat_time := case_times[i]]
      d_match_i[, c("distance", "weights", "subclass") := NULL]
    }

    # update used_as_controls vector
    controls_i <- unique(d_match_i[.treat==FALSE]$.id)
    used_as_controls <- c(used_as_controls, controls_i)

    # append to output
    out[[i]] <- d_match_i

    # save the trace of the functions work
    n_m_controls <- length(controls_i)
    n_cases <- length(ids_cases_i)
    n_pot_controls <- nrow(d_all_i) - n_cases
    trace[[i]] <- data.table(time=case_times[i],
                             new_cases=n_cases,
                             matched_controls=n_m_controls,
                             potential_controls=n_pot_controls)

    if (verbose) {
      cat("Matched ", n_m_controls, " unique controls to ",
          n_cases, " cases (with ", n_pot_controls,
          " potential unique controls) at t = ", case_times[i], ".\n", sep="")
    }
  }

  # full dataset
  data <- rbindlist(out)
  rm(out)

  # create new .id_new to differentiate between persons
  data[, .id_new := .I]

  # for controls, add time of next treatment
  #colnames(d_treat)[colnames(d_treat)==time] <- ".next_treat_time"
  setnames(d_treat, old=".time", new=".next_treat_time")
  data <- merge.data.table(data, d_treat, by=".id", all.x=TRUE)
  data[.treat==TRUE, .next_treat_time := NA]

  # change order of columns
  first_cols <- c(".id", ".id_new", ".id_pair", ".treat", ".treat_time",
                  ".next_treat_time")
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
                        n_orig=length(unique(d_covars[[id]])),
                        n_matched=nrow(data),
                        n_unmatched=sum(!unique(d_covars[[id]]) %in%
                                                 data[[id]]),
                        added_events=c(),
                        added_next_time=c()),
              trace=rbindlist(trace))
  class(out) <- "match_td"

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
