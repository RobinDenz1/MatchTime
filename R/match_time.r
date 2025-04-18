
## main function of the package, but is only a wrapper around the true
## estimation function called match_time.fit()
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @export
match_time <- function(formula, data, id, inclusion=NA,
                       outcomes=NA, start="start", stop="stop",
                       method=c("brsm", "psm", "pgm", "dsm", "greedy"),
                       replace_over_t=FALSE, replace_at_t=FALSE,
                       replace_cases=TRUE, estimand="ATT", ratio=1,
                       recruitment_start=NULL, recruitment_stop=NULL,
                       match_method="fast_exact", matchit_args=list(),
                       save_matchit=FALSE, censor_at_treat=TRUE,
                       censor_pairs=FALSE, units="auto", verbose=FALSE,
                       ...) {

  .treat <- .time <- NULL

  # coerce to data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  # check inputs
  check_inputs_match_time(formula=formula, data=data, id=id,
                          inclusion=inclusion, outcomes=outcomes,
                          replace_over_t=replace_over_t,
                          replace_at_t=replace_at_t,
                          replace_cases=replace_cases,
                          estimand=estimand, ratio=ratio,
                          match_method=match_method,
                          verbose=verbose,
                          start=start, stop=stop, method=method,
                          recruitment_start=recruitment_start,
                          recruitment_stop=recruitment_stop)

  # extract needed things from formula
  form <- process_formula(formula)

  # extract outcome info, if available
  if (all(!is.na(outcomes))) {

    # handle 'event' argument if needed
    if (methods::hasArg("event")) {
      event <- list(...)$event
      outcomes_remove <- outcomes[outcomes != event]
      outcomes <- unique(c(outcomes, event))
    } else {
      outcomes_remove <- outcomes
    }

    l_events <- lapply(outcomes, FUN=times_from_start_stop,
                       id=id, type="event", time_name=".time",
                       data=data)
    data[, (outcomes_remove) := NULL]
  }

  # remove data not relevant for recruitment period
  if (!is.null(recruitment_start) && !is.null(recruitment_stop)) {
    data <- subset_start_stop(data=data, first_time=recruitment_start,
                              last_time=recruitment_stop, truncate=TRUE,
                              start=start, stop=stop)
  }

  # fix treatment variable if needed
  setnames(data, old=form$treat, new=".treat")
  if (!is.logical(data$.treat)) {
    data[, .treat := preprocess_treat(.treat)]
  }

  # extract relevant treatment times
  d_treat <- times_from_start_stop(data=data, name=".treat", id=id,
                                   type="var", time_name=".time",
                                   start=start, stop=stop)
  if (method[1] != "dynamic") {
    data[, .treat := NULL]
  }

  check_treatment(data=d_treat, id=id, method=method[1])

  # call function that does all the work
  out <- match_time.fit(id=id, time=".time", d_treat=d_treat,
                        d_covars=data, match_vars=form$match_vars,
                        replace_over_t=replace_over_t,
                        replace_at_t=replace_at_t, replace_cases=replace_cases,
                        estimand=estimand, ratio=ratio,
                        match_method=match_method, verbose=verbose,
                        start=start, stop=stop, save_matchit=save_matchit,
                        method=method[1], matchit_args=matchit_args,
                        inclusion=inclusion, ...)

  # add call to output
  out$call <- match.call()

  # add back outcomes if specified
  if (all(!is.na(outcomes))) {
    for (i in seq_len(length(outcomes))) {
      out <- add_outcome(out, data=l_events[[i]],
                         id=id, time=".time",
                         event_time_name=paste0(outcomes[i], "_time"),
                         status_name=paste0(outcomes[i], "_status"),
                         censor_at_treat=censor_at_treat,
                         censor_pairs=censor_pairs,
                         units=units)
    }
    if (methods::hasArg("event")) {
      out$data[, (event) := NULL]
    }
  }

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
match_time.fit <- function(id, time, d_treat, d_covars, match_vars=NULL,
                           method="brsm", replace_over_t=FALSE,
                           replace_at_t=FALSE, replace_cases=TRUE,
                           estimand="ATT", ratio=1,
                           match_method="fast_exact",
                           verbose=FALSE, start="start", stop="stop",
                           save_matchit=FALSE, matchit_args=list(),
                           ps_type=c("ps", "lp"), basehaz_interpol="constant",
                           standardize_ps=FALSE, prog_type=c("p", "lp"),
                           standardize_prog=FALSE, event=NA,
                           formula_ps=NULL, formula_prog=NULL,
                           remove_ps=FALSE, remove_prog=FALSE,
                           inclusion=NA) {

  .treat <- .id_pair <- .subclass <- .treat_time <- .strata <- .start <-
    .id_new <- .next_treat_time <- .time <- .stop <- .id <-
    .fully_matched <- .weights <- ..id.. <- .distance <- .ps_score <-
    .lp_ps <- .prog_score <- .lp_prog <- .inclusion <- .remove_all <-
    .treat_at_0 <- .treat_stop <- NULL

  # save sample sizes of input
  n_input_all <- length(unique(d_covars[[id]]))
  n_input_cases <- nrow(d_treat)
  n_input_controls <- n_input_all - nrow(d_treat[.time==0])

  if (method=="greedy") {
    replace_over_t <- TRUE
  }

  # event must be specified with method "pgm" or "dsm"
  stopifnotm(!((method=="pgm" | method=="dsm") && is.na(event)),
             "'event' must be specified when using method='pgm' or",
             "method='dsm'.")

  # rename id / time to prevent possible errors with get()
  setnames(d_treat, old=c(id, time), new=c(".id", ".time"))
  setnames(d_covars, old=c(id, start, stop),
           new=c(".id", ".start", ".stop"))

  # get variables that should be matched on
  if (is.null(match_vars)) {
    cnames <- colnames(d_covars)
    match_vars <- cnames[!cnames %fin% c(".id", ".time", ".treat", ".start",
                                         ".stop", inclusion)]
  }
  cnames <- colnames(d_covars)
  select_vars <- cnames[!cnames %fin% c(".start", ".stop", inclusion)]

  if (match_method %in% c("none", "fast_exact")) {
    select_vars <- c(select_vars, ".strata")
  }

  select_vars <- update_select_vars(select_vars=select_vars,
                                    method=method[1],
                                    ps_type=ps_type,
                                    prog_type=prog_type)

  # estimate propensity score model, if specified
  if (method=="psm" | method=="dsm") {
    ps_model <- fit_ps_model(data=d_covars, d_treat=d_treat,
                             match_vars=match_vars, formula=formula_ps)
  }

  # estimate prognostic score model, if specified
  if (method=="pgm" | method=="dsm") {
    prog_model <- fit_prog_model(data=d_covars, d_treat=d_treat,
                                 match_vars=match_vars, event=event,
                                 formula=formula_prog)
  }

  # time at treatment
  if (method != "dynamic") {
    d_covars <- merge.data.table(d_covars, d_treat, by=".id", all.x=TRUE)
  }

  # time treatment stops, needed for dynamic allocation only
  if (method=="dynamic") {
    d_treat_stop <- simplify_start_stop(data=d_covars, id=".id", start=".start",
                                        stop=".stop", cols=".treat")
    d_treat_stop <- d_treat_stop[.treat==TRUE][, c(".id", ".stop"), with=FALSE]
  }

  # applying inclusion criteria
  if (!all(is.na(inclusion))) {
    l_incl <- apply_inclusion_criteria(d_covars=d_covars,
                                       inclusion=inclusion,
                                       method=method)
    d_covars <- l_incl$d_covars
  }

  # get maximum follow-up time per person (used for adding events later)
  d_longest <- d_covars[, (.max_t = max(.stop)), by=".id"]
  colnames(d_longest) <- c(id, ".max_t")

  # keep only cases that meet inclusion criteria at treatment time
  if (method != "dynamic") {
    include <- d_covars[.time >= .start & .time < .stop]$.id
    d_treat_orig <- copy(d_treat)
    d_treat <- d_treat[.id %fin% include]
  } else {
    d_treat_orig <- d_treat
  }

  # save sample sizes after applying inclusion criteria
  n_incl_all <- length(unique(d_covars$.id))
  n_incl_cases <- nrow(d_treat)
  n_incl_controls <- n_incl_all - nrow(d_treat[.time==0])

  # identify all points in time at which at least one case happened
  case_times <- sort(unique(d_treat$.time))

  # remove time durations after treatment onset
  if (method != "dynamic") {
    min_t_passed <- as.numeric(
      min(shift(case_times, type="lead") - case_times, na.rm=TRUE)
    )
    d_covars <- subset_start_stop(data=d_covars,
                                  last_time=d_covars$.time + min_t_passed,
                                  start=".start", stop=".stop")
    d_covars[, .time := NULL]
  }

  # set up .strata for fast_exact_matching(), if used
  if (match_method=="none") {
    d_covars[, .strata := TRUE]
  } else {
    d_covars[, .strata := do.call(paste0, .SD), .SDcols=match_vars]
  }

  # set up propensity score, if specified
  if (method=="psm" | method=="dsm") {
    h0_ps <- set_propensity_score(d_covars=d_covars, ps_model=ps_model,
                                  ps_type=ps_type,
                                  standardize_ps=standardize_ps,
                                  basehaz_interpol=basehaz_interpol)
  }

  # set up prognostic score, if specified
  if (method=="pgm" | method=="dsm") {
    h0_prog <- set_prognostic_score(d_covars=d_covars, prog_model=prog_model,
                                    prog_type=prog_type,
                                    standardize_prog=standardize_prog,
                                    basehaz_interpol=basehaz_interpol)
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

    if (method=="dynamic") {

      # everyone fulfilling inclusion criteria at t
      d_all_i <- d_covars[
        case_times[i] >= .start & case_times[i] < .stop
      ][, select_vars, with=FALSE]

      # remove current but not new cases
      d_all_i <- d_all_i[!(.treat==TRUE & !.id %fin% ids_cases_i)]

      # if specified, remove those already used as controls
      if (!replace_over_t) {
        d_all_i <- d_all_i[.treat==TRUE | !.id %fin% used_as_controls]
      }

    } else {

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
    }

    # for propensity score matching, calculate .ps_score
    if ((method=="psm" | method=="dsm") & ps_type[1]=="ps") {
      set_score_at_t(data=d_all_i, t=case_times[i], h0=h0_ps,
                     name_score=".ps_score", name_lp=".lp_ps",
                     standardize=standardize_ps)
    }
    if ((method=="pgm" | method=="dsm") & prog_type[1]=="p") {
      set_score_at_t(data=d_all_i, t=case_times[i], h0=h0_prog,
                     name_score=".prog_score", name_lp=".lp_prog",
                     standardize=standardize_prog)
    }

    # perform the actual matching at t
    match_i <- match_at_t(d_all_i=d_all_i, case_times=case_times, i=i,
                          method=method, ids_cases_i=ids_cases_i,
                          match_method=match_method, replace_at_t=replace_at_t,
                          ratio=ratio, match_vars=match_vars,
                          matchit_args=matchit_args, save_matchit=save_matchit,
                          ps_type=ps_type, prog_type=prog_type,
                          remove_ps=remove_ps, remove_prog=remove_prog)
    d_match_i <- match_i$d_match_i

    if (!is.null(match_i$matchit_obj)) {
      matchit_out[[i]] <- match_i$matchit_obj
    }
    rm(match_i)

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
  data <- add_next_time_data(data=data, d_event=d_treat_orig, id=".id",
                             time=".time", include_same_t=FALSE,
                             name=".next_treat_time")

  # in dynamic matching, add time of treatment stop for all treatment periods
  if (method=="dynamic") {
    data <- add_next_time_data(data=data, d_event=d_treat_stop, id=".id",
                               time=".stop", include_same_t=FALSE,
                               name=".treat_stop")
    data[.treat==FALSE, .treat_stop := NA]
  }

  # calculate number of actually matched individuals
  if (".id_pair" %in% colnames(data)) {
    data[, .fully_matched := .N == (ratio + 1), by=.id_pair]
  } else {
    data[, .fully_matched := TRUE]
  }

  n_matched_cases <- nrow(data[.fully_matched==TRUE & .treat==TRUE])
  n_matched_controls <- nrow(data[.fully_matched==TRUE & .treat==FALSE])

  # change order of columns
  set_cols_matchdata(data=data, d_covars=d_covars, id=id)

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
                        method=method,
                        match_method=match_method,
                        match_vars=match_vars,
                        inclusion=inclusion,
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
                         n_incl_controls=n_incl_controls,
                         n_input_all=n_input_all,
                         n_input_cases=n_input_cases,
                         n_input_controls=n_input_controls),
              trace=rbindlist(trace),
              exclusion=list(stage1=NULL, stage2=NULL),
              matchit_objects=NULL,
              ps_model=NULL,
              prog_model=NULL)
  class(out) <- "match_time"

  if (save_matchit & !match_method %in% c("none", "fast_exact") &
      method != "greedy") {
    names(matchit_out) <- as.character(case_times)
    out$matchit_objects <- matchit_out
  }

  if (!all(is.na(inclusion))) {
    out$exclusion <- list(stage1=l_incl$stage1,
                          stage2=l_incl$stage2)
  }

  if (method=="psm" | method=="dsm") {
    out$ps_model <- ps_model
  }
  if (method=="pgm" | method=="dsm") {
    out$prog_model <- prog_model
  }

  return(out)
}

## perform the matching at a specific point in time
#' @importFrom data.table :=
match_at_t <- function(d_all_i, case_times, i, method, ids_cases_i,
                       match_method, replace_at_t, ratio, match_vars,
                       matchit_args, save_matchit, ps_type, prog_type,
                       remove_ps, remove_prog) {

  .treat_time <- .weights <- .strata <- .id_pair <- weights <- ..id.. <-
    .subclass <- .distance <- .treat <- NULL

  out <- list()

  # simply take the entire dataset for method="greedy"
  if (method=="greedy") {
    d_match_i <- d_all_i
    d_match_i[, .treat_time := case_times[i]]
    d_match_i[, .weights := NA]

    if (".strata" %in% colnames(d_match_i)) {
      d_match_i[, .strata := NULL]
    }
  # return only cases if no controls left
  } else if (nrow(d_all_i)==length(ids_cases_i)) {

    d_match_i <- subset(d_all_i, .treat==TRUE)
    d_match_i[, .treat_time := case_times[i]]
    d_match_i[, .weights := 0]

    if (match_method %in% c("none", "fast_exact")) {
      d_match_i[, .strata := NULL]
    }

    # add .id_pair if needed
    if (match_method %in% c("none", "fast_exact", "nearest", "full",
                            "genetic")) {
      d_match_i[, .id_pair := paste0(i, "_", seq_len(.N))]
    }

    set_remove_cols(data=d_match_i, method=method, ps_type=ps_type,
                    prog_type=prog_type, remove_ps=remove_ps,
                    remove_prog=remove_prog)

    # fast exact or no matching
  } else if (match_method=="fast_exact" || match_method=="none") {

    # perform exact matching
    d_match_i <- fast_exact_matching.fit(
      d_all_i,
      treat=".treat",
      strata=".strata",
      estimand="ATT",
      replace=replace_at_t,
      if_no_match="nothing",
      ratio=ratio
    )

    d_match_i[, .id_pair := paste0(i, "_", .id_pair)]
    d_match_i[, .treat_time := case_times[i]]
    d_match_i[, .strata := NULL]

  } else {

    requireNamespace("MatchIt")

    # create formulas for matchit call
    main_formula <- get_main_formula(method, match_vars)

    # perform matching on baseline covariates
    args <- list(formula=stats::as.formula(main_formula),
                 data=d_all_i,
                 method=match_method,
                 estimand="ATT",
                 replace=replace_at_t,
                 ratio=ratio)
    args <- c(args, matchit_args)

    # call matchit(), returning NULL if no matches were found
    d_matchit_i <- tryCatch({
      do.call(MatchIt::matchit, args=args)},
      error=function(e){
        if (e$message=="No matches were found.") {
          return(NULL)
        } else {
          stop(e)
        }
      }
    )

    # if no matches found, just take all cases
    if (is.null(d_matchit_i)) {
      d_match_i <- subset(d_all_i, .treat==TRUE)
      d_match_i[, .treat_time := case_times[i]]
      d_match_i[, .weights := 0]

      # add .id_pair if needed
      if (match_method %in% c("nearest", "full", "genetic")) {
        d_match_i[, .id_pair := paste0(i, "_", seq_len(.N))]
      }

      set_remove_cols(data=d_match_i, method=method, ps_type=ps_type,
                      prog_type=prog_type, remove_ps=remove_ps,
                      remove_prog=remove_prog)
    } else {

      if (save_matchit) {
        out$matchit_obj <- d_matchit_i
      }

      # extract matched data, conditional on method used
      # NOTE: unmatched cases are included as well to be consistent with
      #       other methods
      if (!match_method %in% c("nearest", "full", "genetic")) {
        d_match_i <- MatchIt::match.data(d_matchit_i,
                                         weights=".weights",
                                         subclass=".subclass",
                                         distance=".distance",
                                         drop.unmatched=FALSE)
        d_match_i <- subset(d_match_i, .treat | (!.treat & .weights != 0))
      } else {
        d_match_i <- MatchIt::get_matches(d_matchit_i,
                                          weights=".weights",
                                          subclass=".subclass",
                                          distance=".distance",
                                          id="..id..")

        # add the unmatched cases back in, if required
        if (sum(d_match_i$.treat) != sum(d_all_i$.treat)) {
          d_unmatched_i <- MatchIt::match.data(d_matchit_i,
                                               weights=".weights",
                                               subclass=".subclass",
                                               distance=".distance",
                                               drop.unmatched=FALSE)
          d_unmatched_i <- as.data.table(
            subset(d_unmatched_i, .treat & .weights == 0)
          )

          d_unmatched_i[, .subclass := seq_len(.N) +
                          max(as.numeric(as.character(d_match_i$.subclass)))]
          d_match_i <- rbindlist(list(d_match_i, d_unmatched_i), fill=TRUE)
        }

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

      set_remove_cols(data=d_match_i, method=method, ps_type=ps_type,
                      prog_type=prog_type, remove_ps=remove_ps,
                      remove_prog=remove_prog)

      # add matched time
      d_match_i[, .treat_time := case_times[i]]
    }
  }

  out$d_match_i <- d_match_i

  return(out)
}
