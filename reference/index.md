# Package index

## Time-Dependent Matching

Perform, visualize and inspect time-dependent matching

- [`match_time()`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  : Matching for Causal Inference with Time-Dependent Treatments

- [`method_brsm`](https://robindenz1.github.io/MatchTime/reference/method_brsm.md)
  : Balanced Risk Set Matching

- [`method_psm`](https://robindenz1.github.io/MatchTime/reference/method_psm.md)
  : Time-Dependent Propensity Score Matching

- [`method_pgm`](https://robindenz1.github.io/MatchTime/reference/method_pgm.md)
  : Time-Dependent Prognostic Score Matching

- [`method_dsm`](https://robindenz1.github.io/MatchTime/reference/method_dsm.md)
  : Time-Dependent Double Score Matching

- [`method_greedy`](https://robindenz1.github.io/MatchTime/reference/method_greedy.md)
  : Time-Dependent Greedy Selection of Controls

- [`summary(`*`<match_time>`*`)`](https://robindenz1.github.io/MatchTime/reference/summary.match_time.md)
  :

  View a balance summary of a `match_time` object

- [`bal.tab(`*`<match_time>`*`)`](https://robindenz1.github.io/MatchTime/reference/bal.tab.match_time.md)
  :

  Display Balance Statistics in a Table for `match_time` objects

- [`plot(`*`<match_time>`*`)`](https://robindenz1.github.io/MatchTime/reference/plot.match_time.md)
  : Plot cumulative number of matched cases and controls

- [`plot_flowchart()`](https://robindenz1.github.io/MatchTime/reference/plot_flowchart.md)
  :

  Plot a flowchart of the sample size flow in a `match_time` object

- [`plot_timeline()`](https://robindenz1.github.io/MatchTime/reference/plot_timeline.md)
  :

  Plot the time under observation for ids in a `match_time` object

- [`get_match_data()`](https://robindenz1.github.io/MatchTime/reference/get_match_data.md)
  :

  Construct a matched dataset from a `match_time` object

## Adding Covariates

Adding variables or outcomes to matched data

- [`add_outcome()`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md)
  :

  Add a Time-to-Event Outcome to a `match_time` object

- [`add_next_time()`](https://robindenz1.github.io/MatchTime/reference/add_next_time.md)
  :

  Add the next time of an event to a `match_time` object

- [`add_previous_event()`](https://robindenz1.github.io/MatchTime/reference/add_previous_event.md)
  :

  Add an indicator whether an event was occuring at baseline to a
  `match_time` object

- [`add_previous_event_count()`](https://robindenz1.github.io/MatchTime/reference/add_previous_event_count.md)
  :

  Add a count of the number of events occured before inclusion to a
  `match_time` object

- [`add_from_start_stop()`](https://robindenz1.github.io/MatchTime/reference/add_from_start_stop.md)
  :

  Add information from start-stop data to a `match_time` object

## Start-Stop Data

Creating and transforming start-stop data

- [`merge_start_stop()`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
  : Merge two or more datasets in the start-stop format
- [`simplify_start_stop()`](https://robindenz1.github.io/MatchTime/reference/simplify_start_stop.md)
  : Combines rows with the same values in start-stop data
- [`subset_start_stop()`](https://robindenz1.github.io/MatchTime/reference/subset_start_stop.md)
  : Subsetting start-stop format datasets
- [`times_from_start_stop()`](https://robindenz1.github.io/MatchTime/reference/times_from_start_stop.md)
  : Extract "event" times from start-stop format datasets
- [`long2start_stop()`](https://robindenz1.github.io/MatchTime/reference/long2start_stop.md)
  : Transform a long-format dataset into the start-stop format
- [`start_stop2long()`](https://robindenz1.github.io/MatchTime/reference/start_stop2long.md)
  : Transform a start-stop dataset into the long-format
- [`fill_gaps_start_stop()`](https://robindenz1.github.io/MatchTime/reference/fill_gaps_start_stop.md)
  : Add missing intervals to incomplete start-stop data

## Misc

- [`fast_exact_matching()`](https://robindenz1.github.io/MatchTime/reference/fast_exact_matching.md)
  : Fast Exact Matching with a Binary Treatment

- [`stratified_sample()`](https://robindenz1.github.io/MatchTime/reference/stratified_sample.md)
  :

  Fast Stratified Sampling from a `data.table` like object
