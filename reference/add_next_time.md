# Add the next time of an event to a `match_time` object

In some cases, some relevant events occur after the time at which a case
was included during matching using the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function. These times may correspond to some censoring event, such as
time to death etc. This function allows users to add this time directly
to the matched data. Contrary to the
[`add_outcome`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md)
function, it only adds one column to the data that actually contains the
time of the "event", not the time until the event as measured from study
entry.

## Usage

``` r
add_next_time(x, data, id=x$id, time=x$time,
              include_same_t=TRUE,
              name=".next_time")
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
  as a `data.frame` or a `tibble`. The time of the next event per case
  will be added to the matched data. Cases with no events can simply be
  omitted from `data`. If multiple events per person exist, they should
  be included in the long-format (multiple rows per `id`). Only the time
  of the first event after inclusion into the matching data will be
  added, all previous events and all events afterwards are ignored.

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

- include_same_t:

  Either `TRUE` or `FALSE`, specifying whether the time of inclusion
  (`.treat_time` in `x$data`) should be included when adding the next
  event time. If `TRUE` (default), an event happening exactly at the
  time of inclusion will be considered the next event. If `FALSE`, an
  event happening exactly at the time of inclusion will be considered a
  "past event" instead. In the latter case, only events strictly after
  inclusion time are added to the matched data.

- name:

  A single character string specifying the name of the column containing
  the next event time that will be added to the `data` object contained
  in `x`. Defaults to `.next_time`. If the name is already present, an
  error message is returned instead.

## Details

On first glance it might be confusing whether to use `add_next_time` or
[`add_outcome`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md),
but the distinction is actually quite simple. Use `add_next_time`
whenever you want to add the *actual time of occurence of the next event
after inclusion time* and use
[`add_outcome`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md)
whenever you are interested in adding the *time until the next event as
measured from the inclusion time* (possible subject to censoring). As
the name suggests, the latter is usually used for time-to-event
outcomes, while the former is usually used to add extra information
required to add more sorts of right-censoring etc.

## Value

Returns a modified `match_time` object. It is essentially the same
object as the supplied `x`, but it also contains a new column: `name`
(the time of occurence of the next event).

## Author

Robin Denz

## See also

[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
[`add_outcome`](https://robindenz1.github.io/MatchTime/reference/add_outcome.md)

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
#       for showcasing the functionality of add_next_time()
d_events <- data.table(id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       time=c(50, 6, 16, 39, 18, 3, 675, 40, 85, 58))

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                    match_method="nearest")

# add the time of next event to match_time object
m_obj <- add_next_time(m_obj, data=d_events, time="time")
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
#>    .weights        age surgery .next_time
#>       <num>      <num>   <num>      <num>
#> 1:        1 -17.155373       0         50
#> 2:        1   6.297057       0         16
#> 3:        1  -7.737166       0         39
#> 4:        1  -7.737166       0         39
#> 5:        1   2.869268       0        675
#> 6:        0   2.869268       0        675
```
