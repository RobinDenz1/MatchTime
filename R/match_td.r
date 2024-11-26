
## main function of the package, but is only a wrapper around the true
## estimation function called match_td.fit()
#' @importFrom data.table is.data.table
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @export
match_td <- function(formula, data, id, inclusion=NA, event=NA,
                     replace_over_t=FALSE, replace_at_t=FALSE,
                     replace_cases=TRUE, estimand="ATT", ratio=1,
                     if_no_match="stop", censor_at_treat=TRUE,
                     censor_pairs=TRUE, match_method="fast_exact",
                     keep_all_columns=FALSE, units="auto", verbose=FALSE,
                     ...) {

  # coerce to data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  # check inputs
  check_inputs_match_td(formula=formula, data=data, id=id,
                        inclusion=inclusion, event=event,
                        replace_over_t=replace_over_t,
                        replace_at_t=replace_at_t,
                        replace_cases=replace_cases,
                        estimand=estimand, ratio=ratio,
                        if_no_match=if_no_match,
                        censor_at_treat=censor_at_treat,
                        censor_pairs=censor_pairs,
                        match_method=match_method,
                        verbose=verbose,
                        keep_all_columns=keep_all_columns)

  # extract needed things from formula
  vars <- all.vars(formula)
  treat <- vars[1]
  if (length(vars) > 1) {
    match_vars <- vars[2:length(vars)]
  } else {
    match_vars <- NULL
  }

  # extract relevant treatment times
  d_treat <- times_from_start_stop(data=data, name=treat, id=id,
                                   type="var", time_name=".time")
  data[, (treat) := NULL]

  # extract relevant event times
  if (!is.na(event)) {
    d_event <- times_from_start_stop(data=data, name=event, id=id,
                                     type="event", time_name=".time")
    data[, (event) := NULL]
  } else {
    d_event <- NULL
  }

  # remove all rows when inclusion criteria are not met
  if (!is.na(inclusion)) {
    data <- data[eval(inclusion)==TRUE]
    data[, (inclusion) := NULL]
  }

  # call function that does all the work
  out <- match_td.fit(id=id, time=".time", d_treat=d_treat,
                      d_event=d_event, d_covars=data,
                      match_vars=match_vars, replace_over_t=replace_over_t,
                      replace_at_t=replace_at_t, replace_cases=replace_cases,
                      censor_pairs=censor_pairs, estimand=estimand,
                      ratio=ratio, if_no_match=if_no_match,
                      match_method=match_method, units=units,
                      keep_all_columns=keep_all_columns, verbose=verbose)

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
#' @export
match_td.fit <- function(id, time, d_treat, d_event, d_covars,
                         match_vars=NULL, replace_over_t=FALSE,
                         replace_at_t=FALSE, replace_cases=TRUE,
                         censor_at_treat=TRUE, censor_pairs=TRUE,
                         estimand="ATT", ratio=1,
                         if_no_match="stop", match_method="fast_exact",
                         keep_all_columns=FALSE, units="auto",
                         verbose=FALSE, ...) {

  start <- .treat <- pair_id <- subclass <- .treat_time <- .strata <-
    .id_new <- .next_treat_time <- .next_event_time <- ..time.. <- NULL

  # rename id / time to prevent possible errors with get()
  setnames(d_treat, old=c(id, time), new=c("..id..", "..time.."))
  setnames(d_covars, old=id, new="..id..")

  orig_id <- id
  orig_time <- time
  id <- "..id.."
  time <- "..time.."

  # get variables that should be matched on
  if (is.null(match_vars)) {
    cnames <- colnames(d_covars)
    match_vars <- cnames[!cnames %fin% c(id, time, ".treat", "start", "stop")]
  }
  cnames <- colnames(d_covars)
  select_vars <- cnames[!cnames %fin% c("start", "stop")]

  # get maximum follow-up time per person, if needed later
  if (!is.null(d_event)) {
    setnames(d_event, old=c(orig_id, orig_time), new=c("..id..", "..time.."))
    d_longest <- d_covars[, (.max_t = max(stop)), by=eval(id)]
    colnames(d_longest) <- c(id, ".max_t")
  }

  # keep only cases that meet inclusion criteria at treatment time
  d_covars <- merge(d_covars, d_treat, by=id, all.x=TRUE)
  include <- d_covars[get(time) >= start & get(time) < stop][[eval(id)]]
  d_treat <- d_treat[get(id) %fin% include]

  # remove time durations after treatment onset
  d_covars <- subset_start_stop(data=d_covars, last_time=d_covars[[time]] + 1)
  d_covars[, ..time.. := NULL]

  # identify all points in time at which at least one case happened
  case_times <- sort(unique(d_treat[[eval(time)]]))

  # initialize needed id collections
  used_as_controls <- c()
  used_as_cases <- c()

  out <- vector(mode="list", length=length(case_times))
  for (i in seq_len(length(case_times))) {

    # identify new cases at t
    ids_cases_i <- d_treat[get(time)==case_times[i]][[eval(id)]]

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
    ids_pot_controls_i <- unique(d_covars[[eval(id)]])
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
    d_all_i <- d_covars[get(id) %fin% all_ids &
                        case_times[i] >= start & case_times[i] < stop
                        ][, select_vars, with=FALSE]

    # identify cases
    d_all_i[, .treat := get(id) %fin% ids_cases_i]

    ## perform matching at t, if specified
    if (match_method=="none") {

      n_treat <- sum(d_all_i$.treat)
      size <- c(n_treat, n_treat*ratio)
      names(size) <- c("TRUE", "FALSE")

      d_match_i <- stratified_sample(data=d_all_i,
                                     n=size,
                                     strata=".treat",
                                     replace=replace_at_t,
                                     if_lt_n=if_no_match)

      d_match_i[, pair_id := rep(seq_len(n_treat), ratio + 1)]
      d_match_i[, pair_id := paste0(i, "_", pair_id)]
      d_match_i[, .treat_time := case_times[i]]

    } else if (match_method=="fast_exact") {

      # create strata variable
      d_all_i[, .strata := do.call(paste0, .SD), .SDcols=match_vars]

      # perform exact matching
      d_match_i <- fast_exact_matching(d_all_i,
                                       treat=".treat",
                                       strata=".strata",
                                       replace=replace_at_t,
                                       if_lt_n=if_no_match,
                                       ratio=ratio,
                                       check_inputs=FALSE,
                                       copy_data=FALSE)
      d_match_i[, pair_id := paste0(i, "_", pair_id)]
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

      # assign pair_id
      d_match_i <- copy(d_match_i)
      d_match_i[, pair_id := paste0(i, "_", subclass)]
      d_match_i[, .treat_time := case_times[i]]
      d_match_i[, c("distance", "weights", "subclass") := NULL]
    }

    # update used_as_controls vector
    controls_i <- unique(d_match_i[.treat==FALSE][[eval(id)]])
    used_as_controls <- c(used_as_controls, controls_i)

    # append to output
    out[[i]] <- d_match_i

    if (verbose) {
      cat("Matched ", length(controls_i), " unique controls to ",
          length(ids_cases_i), " cases (with ", length(ids_pot_controls_i),
          " potential unique controls) at t = ", case_times[i], ".\n", sep="")
    }
  }

  # full dataset
  data <- rbindlist(out)
  rm(out)

  # create new .id_new to differentiate between persons
  data[, .id_new := .I]

  # for controls, add time of next treatment
  colnames(d_treat)[colnames(d_treat)==time] <- ".next_treat_time"
  data <- merge(data, d_treat, by=id, all.x=TRUE)
  data[.treat==TRUE, .next_treat_time := NA]

  # add event_time and status if specified
  if (!is.null(d_event)) {
    data <- add_tte_outcome(id=id, time=time, data=data, d_event=d_event,
                            censor_pairs=censor_pairs, d_longest=d_longest,
                            censor_at_treat=censor_at_treat, units=units)
    first_cols <- c(id, ".id_new", "pair_id", ".treat", ".treat_time",
                    ".next_treat_time", ".next_event_time", "event_time",
                    "status")
  } else {
    first_cols <- c(id, ".id_new", "pair_id", ".treat", ".treat_time",
                    ".next_treat_time")
  }

  # change order of columns
  last_cols <- colnames(data)[!colnames(data) %fin% first_cols]
  setcolorder(data, c(first_cols, last_cols))
  setnames(data, old=id, new=orig_id)

  # remove some columns if specified
  if (!keep_all_columns) {
    data[, .treat_time := NULL]
    data[, .next_treat_time := NULL]

    if (!is.null(d_event)) {
      data[, .next_event_time := NULL]
    }
  }

  return(data)
}
