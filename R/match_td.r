
# TODO:
# - max_t integrieren
# - in Zukunft: d_inclusion argument ersetzen mit "include" o.ä.
#   - entweder data.table wie jetzt
#   - oder logische Abfrage auf d_covars
# - eine schönere nutzeroberfläche mit formula argument erstellen

## perform time-dependent matching
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table rbindlist
#' @importFrom data.table setcolorder
#' @export
match_td <- function(id, time, d_treat, d_event, d_covars,
                     d_inclusion=NULL, match_vars=NULL,
                     replace_over_t=FALSE, replace_at_t=FALSE,
                     replace_cases=TRUE, censor_pairs=TRUE,
                     estimand="ATT", ratio=1, if_lt_n_at_t="stop",
                     use_matchit=FALSE, matchit_method="nearest",
                     keep_all_columns=FALSE, verbose=FALSE, ...) {

  # get variables that should be matched on
  if (is.null(match_vars)) {
    cnames <- colnames(d_covars)
    match_vars <- cnames[!cnames %fin% c(id, time, ".treat", "start", "stop")]
  }
  select_vars <- c(id, match_vars)

  # apply inclusion criteria at treatment time
  if (!is.null(d_inclusion)) {

    # keep only cases that meet inclusion criteria at treatment time
    d_inclusion <- merge(d_inclusion, d_treat, by=id, all.x=TRUE)
    include <- d_inclusion[eval(parse(text=time)) >= start
                           & eval(parse(text=time)) <= stop][[eval(id)]]
    d_treat <- d_treat[eval(parse(text=id)) %fin% include]
    d_inclusion[, eval(time) := NULL]
  }

  # identify all points in time at which at least one case happened
  case_times <- sort(unique(d_treat[[eval(time)]]))

  # initialize needed id collections
  used_as_controls <- c()
  used_as_cases <- c()

  out <- vector(mode="list", length=length(case_times))
  for (i in seq_len(length(case_times))) {

    # identify new cases at t
    ids_cases_i <- d_treat[eval(parse(text=time))==case_times[i]][[eval(id)]]

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
    if (!is.null(d_inclusion)) {
      ids_pot_controls_i <- d_inclusion[case_times[i] >= start &
                                          case_times[i] <= stop][[eval(id)]]
    } else {
      ids_pot_controls_i <- unique(d_covars[[eval(id)]])
    }
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
    d_all_i <- d_covars[eval(parse(text=id)) %fin% all_ids &
                        case_times[i] >= start & case_times[i] <= stop
                        ][, select_vars, with=FALSE]

    # identify cases
    d_all_i[, .treat := eval(parse(text=id)) %fin% ids_cases_i]

    ## perform matching at t
    if (use_matchit) {

      requireNamespace("MatchIt")

      # create formulas for matchit call
      main_formula <- paste0(".treat ~ ", paste0(match_vars, collapse=" + "))

      # perform matching on baseline covariates
      args <- list(formula=stats::as.formula(main_formula),
                   data=d_all_i,
                   method=matchit_method,
                   estimand=estimand,
                   replace=replace_at_t,
                   ratio=ratio)
      args <- c(args, list(...))

      d_match_i <- do.call(MatchIt::matchit, args=args)
      d_match_i <- MatchIt::match.data(d_match_i)

      # assign pair_id
      d_match_i[, pair_id := paste0(i, "_", subclass)]
      d_match_i[, .treat_time := case_times[i]]
      d_match_i[, c("distance", "weights", "subclass") := NULL]

    } else {

      # create strata variable
      d_all_i[, .strata := do.call(paste0, .SD), .SDcols=match_vars]

      # perform exact matching
      d_match_i <- fast_exact_matching(d_all_i,
                                       treat=".treat",
                                       strata=".strata",
                                       replace=replace_at_t,
                                       if_lt_n=if_lt_n_at_t,
                                       ratio=ratio)
      d_match_i[, pair_id := paste0(i, "_", pair_id)]
      d_match_i[, .treat_time := case_times[i]]
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

  # add event information
  data <- add_next_event_time(data=data, d_event=d_event, id=id, time=time,
                              include_same_t=TRUE)

  # shift times according to start
  data[, .next_treat_time := .next_treat_time - .treat_time]
  data[, .next_event_time := .next_event_time - .treat_time]

  # get event status indicator
  data[, status := FALSE]
  data[is.na(.next_treat_time) & !is.na(.next_event_time), status := TRUE]
  data[.next_treat_time > .next_event_time, status := TRUE]

  # calculate corresponding event time
  # TODO: 365 should not be hardcoded here
  data[, event_time := pmin(.next_treat_time, .next_event_time,
                            365 - .treat_time, na.rm=TRUE)]

  # if specified and a control is censored because it became a case later,
  # also censor the corresponding pair to which it is a control at the same time
  # NOTE: does this work with ratio > 1?
  if (censor_pairs) {

    # identify all such cases
    d_cens <- data[.next_treat_time < .next_event_time]
    d_cens <- d_cens[, c("pair_id", "event_time"), with=FALSE]
    colnames(d_cens) <- c("pair_id", "pair_id_event_time")

    # merge them to data, update variables accordingly
    data <- merge(data, d_cens, by="pair_id", all.x=TRUE)
    data[pair_id_event_time < event_time, status := FALSE]
    data[pair_id_event_time < event_time, event_time := pair_id_event_time]
    data[, pair_id_event_time := NULL]
  }

  data[, .strata := NULL]

  # change order of columns
  first_cols <- c(id, ".id_new", "pair_id", ".treat", ".treat_time",
                  ".next_treat_time", ".next_event_time", "event_time",
                  "status")
  last_cols <- colnames(data)[!colnames(data) %fin% first_cols]
  setcolorder(data, c(first_cols, last_cols))

  if (!keep_all_columns) {
    data[, .treat_time := NULL]
    data[, .next_treat_time := NULL]
    data[, .next_event_time := NULL]
  }

  return(data)
}

## given the current matched data and a data.table containing none, one or
## multiple events per person, add the next event after .treat_time
add_next_event_time <- function(data, d_event, id, time,
                                include_same_t=TRUE) {

  # merge to matched data, creating new rows
  colnames(d_event)[colnames(d_event)==time] <- ".next_event_time"
  data <- merge(data, d_event, by=id, all.x=TRUE, allow.cartesian=TRUE)

  # check if event is after inclusion time
  # NOTE: the way I simulated the data this argument should be TRUE for
  #       outcomes, but FALSE for time-dependent variables
  #       (although it is unclear why it would be used for the latter)
  if (include_same_t) {
    data[, is_after := .next_event_time >= .treat_time, by=.id_new]
  } else {
    data[, is_after := .next_event_time > .treat_time, by=.id_new]
  }

  # calculate time of first influenza after inclusion time
  data[is_after==FALSE, .next_event_time := NA]
  data[, .next_event_time := min(.next_event_time, na.rm=TRUE), by=.id_new]
  data[, is_after := NULL]

  # remove duplicate rows
  data <- unique(data)

  return(data)
}

## given a data.table containing event times per person and a duration of
## that event, adds an indicator to the matched data which is TRUE if the
## event was currently going on when the person got matched and FALSE otherwise
add_previous_event_time <- function(data, d_prev, id, time, duration,
                                    name, include_same_t=FALSE) {

  # merge to matched data, creating new rows
  colnames(d_prev)[colnames(d_prev)==time] <- ".prev_time"
  data <- merge(data, d_prev, by=id, all.x=TRUE)

  # check if .treat_time is in any of the previous risk periods by event
  # NOTE: the way I simulated the data this argument should be TRUE for
  #       time-dependent covariates, but FALSE for outcomes
  if (include_same_t) {
    data[, .in_risk := .treat_time < (.prev_time + eval(duration)) &
           .treat_time >= .prev_time]
  } else {
    data[, .in_risk := .treat_time < (.prev_time + eval(duration)) &
           .treat_time > .prev_time]
  }

  data[is.na(.in_risk), .in_risk := FALSE]
  data[, .in_risk := any(.in_risk), by=.id_new]
  data[, .prev_time := NULL]

  # remove duplicate rows
  data <- unique(data)
  colnames(data)[colnames(data)==".in_risk"] <- name

  return(data)
}
