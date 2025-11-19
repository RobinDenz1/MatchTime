# Add a Time-to-Event Outcome to a `match_time` object

After performing time-dependent matching using the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function, users usually want to add one or more outcomes to the data.
This function may be used to do this efficiently, potentially using
different censoring schemes. This is equivalent to including the outcome
in the start-stop dataset supplied in the matching process and
specifying it in the `outcomes` argument.

## Usage

``` r
add_outcome(x, data, censor_at_treat=TRUE,
            censor_pairs=FALSE, units="auto",
            id=x$id, time=x$time,
            event_time_name=".event_time",
            status_name=".status")
```

## Arguments

- x:

  A `match_time` object created using the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- data:

  A `data.table` like object including exactly two columns: `id` (the
  unique case identifier), `time` (the time at which an event occurred).
  May also be any object that can be coerced to be a `data.table`, such
  as a `data.frame` or a `tibble`. The event times supplied using this
  argument will then be put into relation with the time at which each
  case was included during the matching process, giving the time until
  the event. Cases with no events can simply be omitted from `data`. If
  multiple events per person exist, they should be included in the
  long-format (multiple rows per `id`). Only the time until the first
  event after inclusion into the matching data will be used, all
  previous events and all events afterwards are not used.

- censor_at_treat:

  Either `TRUE` or `FALSE`, indicating whether the created event time
  should be censored at the time of the next treatment. This only
  applies to cases that were included in the matching process as
  controls but later become cases themselves. Defaults to `TRUE`.

- censor_pairs:

  Either `TRUE` or `FALSE`. Only used if `censor_at_treat=TRUE`. If set
  to `TRUE`, the case matched to a control is censored at the same time
  that the control was censored due to the next treatment occurring. If
  `ratio > 1`, the minimum time to "artificial censoring" is used as
  censoring time for all cases with the same `.id_pair`. This may in
  some cases be sufficient to deal with the covariate dependent
  censoring induced by using `censor_at_treat=TRUE`.

- units:

  Corresponds to the argument of the same name in the
  [`difftime`](https://rdrr.io/r/base/difftime.html) function. This
  argument is only used when the `time` column corresponds to a `Date`
  (or similar) variable. It should be used to indicate the time-scale of
  the created event time (seconds, days, years, ...).

- id:

  A single character string specifying a column in `data`, specifying
  the unique case identifier. By default the same name that was used in
  the original
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  is used here.

- time:

  A single character string specifying a column in `data`, specifying
  the column containing the event times. By default the same name that
  was used in the original
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  is used here.

- event_time_name:

  A single character string specifying the name of the event time column
  that will be added to the `data` object contained in `x`. Defaults to
  `.event_time`. If the name is already present, an error message is
  returned instead.

- status_name:

  A single character string specifying the name of the status column
  that will be added to the `data` object contained in `x`. Defaults to
  `.status`. If the name is already present, an error message is
  returned instead.

## Details

Different censoring schemes can be used for different target estimands.
In intention-to-treat type analysis, users usually should not be
censoring cases at their following treatment times. For per-protocol
type analysis, setting `censor_at_treat` is usually the right decision.
Whether to use `censor_pairs` is an additional choice that depends on
how the user wants to handle the dependent censoring introduced by using
`censor_at_treat=TRUE`.

## Value

Returns a modified `match_time` object. It is essentially the same
object as the supplied `x`, but it also contains two new columns:
`event_time_name` (the time until the event or censoring) and
`status_name` (a logical variable indicating whether the event occurred
or the case was censored).

## Author

Robin Denz

## See also

[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)

## Examples

``` r
library(data.table)
library(MatchTime)

# only execute if packages are available
if (requireNamespace("survival") & requireNamespace("MatchIt")) {

library(survival)
library(MatchIt)

# set random seed to make the output replicably
set.seed(1234)

# load "heart" data from survival package
data("heart")
heart <- heart[, c("id", "start", "stop", "transplant", "age", "surgery")]

# suppose we had an extra dataset with events that looks like this
# NOTE: these are not all events in the real "heart" data and is merely used
#       for showcasing the functionality of add_outcome()
d_events <- data.table(id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       time=c(50, 6, 16, 39, 18, 3, 675, 40, 85, 58))

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                    match_method="nearest")

# add the event times to match_time object
m_obj <- add_outcome(m_obj, data=d_events, time="time")
head(m_obj$data)
}
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Key: <id>
#>       id .id_new .id_pair .treat .treat_time .next_treat_time .fully_matched
#>    <num>   <int>   <char> <lgcl>       <num>            <num>         <lgcl>
#> 1:     1      72       36  FALSE          27               NA           TRUE
#> 2:     3       1        1   TRUE           1               NA           TRUE
#> 3:     4      10        5  FALSE           2               36           TRUE
#> 4:     4      89       45   TRUE          36               NA           TRUE
#> 5:     7      86       43  FALSE          32               51           TRUE
#> 6:     7     105       54   TRUE          51               NA          FALSE
#>    .weights        age surgery .status .event_time
#>       <num>      <num>   <num>  <lgcl>       <num>
#> 1:        1 -17.155373       0    TRUE          23
#> 2:        1   6.297057       0    TRUE          15
#> 3:        1  -7.737166       0   FALSE          34
#> 4:        1  -7.737166       0    TRUE           3
#> 5:        1   2.869268       0   FALSE          19
#> 6:        0   2.869268       0    TRUE         624
```
