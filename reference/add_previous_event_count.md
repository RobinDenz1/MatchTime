# Add a count of the number of events occured before inclusion to a `match_time` object

In some cases, some relevant events occur *before* the time at which a
case was included during matching using the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function. This might be an unrelated treatment or any other
time-dependent variable with a duration. To count how many such "events"
occured in a set duration before inclusion time might be required for
further confounder adjustment or other analysis. If these variables are
already included in the start-stop data supplied to
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
they will be included in the matched data automatically, in which case
this function is not needed. Otherwise, this function offers an easy and
fast way to add such a count to the matched data.

## Usage

``` r
add_previous_event_count(x, data, id=x$id, time=x$time,
                         duration, include_same_t=FALSE,
                         units="auto", name=".prev_event_count")
```

## Arguments

- x:

  A `match_time` object created using the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- data:

  A `data.table` like object including exactly two columns: `id` (the
  unique case identifier), `time` (the time at which an "event"
  occurred). May also be any object that can be coerced to be a
  `data.table`, such as a `data.frame` or a `tibble`. If multiple events
  per person exist, they should be included in the long-format (multiple
  rows per `id`).

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

- duration:

  A single positive number specifying the duration before the inclusion
  time (`.treat_time` in `x$data`) in which the events should be
  counted. For example, setting `duration = 20` means that the number of
  events occuring 20 time units before the inclusion time will be
  counted.

- include_same_t:

  Either `TRUE` or `FALSE` (default), specifying whether the time of
  inclusion (`.treat_time` in `x$data`) should be included when adding
  the next indicator. If `TRUE`, an event occuring exactly at the time
  of inclusion will be considered as a previous event, resulting in the
  added indicator being one point higher. If `FALSE`, an event occuring
  exactly at the time of inclusion will not be considered a previous
  event.

- units:

  Corresponds to the argument of the same name in the
  [`difftime`](https://rdrr.io/r/base/difftime.html) function. This
  argument is only used when the `time` column corresponds to a `Date`
  (or similar) variable. It should be used to indicate the time-scale of
  the `duration` (seconds, days, years, ...).

- name:

  A single character string specifying the name of the column containing
  the count that will be added to the `data` object contained in `x`.
  Defaults to `.prev_event_count`. If the name is already present, an
  error message is returned instead.

## Details

In most cases it is easier and cleaner to just add all variables to the
start-stop data supplied to the `data` argument in
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
regardless of whether matching should be performed on these variables or
not. This way, they will be present in the output data without any
further function calls. In some cases, however, the dataset may be too
large to allow all variables to be present. This might be the case when
the variable changes at many points in time, requiring many rows per
`id`. In these cases it might be necessary to add them later to make the
matching process possible.

## Value

Returns a modified `match_time` object. It is essentially the same
object as the supplied `x`, but it also contains a new column: `name`
(the count of events that occured `duration` time units before the
inclusion time).

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
# NOTE: these are not actual events in the real "heart" data and is merely used
#       for showcasing the functionality of add_previous_event()
d_events <- data.table(id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       time=c(5, 2, 12, 39, 2, 665, 675, 4, 1, 23))

# perform nearest neighbor time-dependent matching on "age" and "surgery"
# (plus exact matching on time)
m_obj <- match_time(transplant ~ age + surgery, data=heart, id="id",
                    match_method="nearest")

# add the count of events occuring 20 time units before inclusion
# to the match_time object
m_obj <- add_previous_event_count(m_obj, data=d_events, time="time",
                                  duration=20)
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
#>    .weights        age surgery .prev_event_count
#>       <num>      <num>   <num>             <int>
#> 1:        1 -17.155373       0                 0
#> 2:        1   6.297057       0                 0
#> 3:        1  -7.737166       0                 0
#> 4:        1  -7.737166       0                 0
#> 5:        1   2.869268       0                 0
#> 6:        0   2.869268       0                 0
```
