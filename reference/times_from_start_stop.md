# Extract "event" times from start-stop format datasets

Given a `data.table` like object in the start-stop format, it returns a
new `data.table` containing the times at which events of a particular
type happened.

## Usage

``` r
times_from_start_stop(data, id, name, type, start="start",
                      stop="stop", time_name="time")
```

## Arguments

- data:

  A `data.table` like object including at least three columns: `id` (the
  unique case identifier), `start` (the beginning of the time-interval)
  and `stop` (the end of the time-interval). May also be any object that
  can be coerced to be a `data.table`, such as a `data.frame` or a
  `tibble`. Intervals should be coded as `[start, stop)`, like in all
  other functions of this package.

- id:

  A single character string specifying the column containing the unique
  case identifier.

- name:

  A single character string specifying the "event" column in `data`. The
  specified column should be of class "logical" (containing only either
  `TRUE` or `FALSE`). Alternatively, the specified variable may be a
  numeric variable containing only 0 (considered `FALSE`) and 1
  (considered `TRUE`).

- type:

  A single character string specifying which type of variable the column
  specified by `name` is. If the variable is an actual event, meaning
  that existing intervals end at the exact time that `name` occured, it
  should be set to `type="event"`. In this case, the `stop` value of all
  intervals where `name` is `TRUE` are extracted. If the variable refers
  to a time-varying binary variable instead (for example a
  time-dependent exposure that can be present or absent), it should be
  set to `"var"`, in which case the `start` time of each duration where
  `name` was `TRUE` are extracted. See details.

- start:

  A single character string specifying a column in `data` specifying the
  beginning of a time-interval. Defaults to `"start"`.

- stop:

  A single character string specifying a column in `data` specifying the
  ending of a time-interval. Defaults to `"stop"`.

- time_name:

  A single character string specifying the name that the `"time"`
  variable should have in the output data. Defaults to `"time"`.

## Value

Returns a single `data.table` containing the subsetted start-stop data.

## Details

This function may be useful to extract times of occurence of binary
time-dependent exposures or actual events from start-stop data.

***Use on Time-Varying Variables***:

If `type="var"` is used the variable specified by `name` is treated as a
simple time-varying variable and only the start times of each
uninterrupted duration where this variable is `TRUE` are extracted. For
example, if the variable starts being `TRUE` at t = 20 and stops being
`TRUE` at t = 123 and it was never `TRUE` before or after these times,
`time_name` would simple be 20 for this individual, regardless of how
many intervals are present where `name` is `TRUE`. This is done because
it is continuously `TRUE` and we only want to extract the initial time
where it "occured" or "happened". In this case, if `name` goes back to
`FALSE` and is `TRUE` again later, for example at t = 700, the output
would contain another entry for this `id` including the time 700,
because this constitutes another occurence.

***Use on actual events***:

If `type="event"` is used instead, every single occurence of `TRUE` in
the input `data` is considered to specify a single event occurence in
`name`, regardless of whether these intervals are directly after one
another. This is the classic difference between coding time-varying
variables and events in start-stop data, as discussed in the survival
package documentation and in the vignettes of this package.

## Author

Robin Denz

## See also

[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md),
[`fill_gaps_start_stop`](https://robindenz1.github.io/MatchTime/reference/fill_gaps_start_stop.md),
[`simplify_start_stop`](https://robindenz1.github.io/MatchTime/reference/simplify_start_stop.md)

## Examples

``` r
library(MatchTime)
library(data.table)

# define some example start-stop data
data <- data.table(id=c(1, 1, 1, 1, 1, 2, 2, 2),
                   start=c(0, 10, 25, 812, 1092, 90, 9023, 10000),
                   stop=c(10, 25, 812, 1092, 34334, 8021, 9823, 220022),
                   exposure=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
                              TRUE))

# treating it as an exposure
# NOTE: in this case, the first two rows of id = 1 are considered to be
#       one continuous occurence, because "exposure" stayed TRUE the entire
#       time
out1 <- times_from_start_stop(data, id="id", name="exposure", type="var")
head(out1)
#> Key: <id, time>
#>       id  time
#>    <num> <num>
#> 1:     1     0
#> 2:     1   812
#> 3:     2 10000

# treating it as an event
# NOTE: in this case the first two rows of id = 1 are considered to be
#       two independent events, events force a time-interval to stop
out2 <- times_from_start_stop(data, id="id", name="exposure", type="event")
head(out2)
#> Key: <id, time>
#>       id   time
#>    <num>  <num>
#> 1:     1     10
#> 2:     1     25
#> 3:     1   1092
#> 4:     2 220022
```
