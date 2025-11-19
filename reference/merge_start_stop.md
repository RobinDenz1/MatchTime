# Merge two or more datasets in the start-stop format

Given multiple `data.table`s (or similar objects) containing partial or
complete information about time periods or intervals (start-stop data),
this function creates a single complete `data.table` in the start-stop
format containing all of this information. Single or recurrent events
may be added. It is essentially a faster and more RAM efficient version
of the classic `tmerge` function of the survival package with some
additional functionality. It is not a generic function or `S3` method,
because the input are `data.table`s.

## Usage

``` r
merge_start_stop(x, y, ..., dlist, by, start="start",
                 stop="stop", all=FALSE, all.x=all,
                 all.y=all, first_time=NULL, last_time=NULL,
                 remove_before_first=TRUE,
                 remove_after_last=TRUE,
                 center_on_first=FALSE, units="auto",
                 defaults=NULL, event_times=NULL,
                 time_to_first_event=FALSE,
                 status="status", constant_vars=NULL)
```

## Arguments

- x, y, ...:

  `data.table` like objects (`tibble`, `data.frame`, ...) containing
  information about time periods. Those should contain information for
  one or more variables to be created. Each `data.table` should contain
  four or more columns: `by` (the individual identifier across all
  `data.table`s), `start` (the beginning of an interval), `stop` (the
  end of an interval) and at least one variable containing some values
  observed during this interval for the respective individual identified
  using `by`. More information is given under "Details". Users should
  define either `x` and `y` (and potentially parse more objects through
  `...`) or define the `dlist` argument instead.

- dlist:

  A list of `data.table` like objects. This argument may be used instead
  of parsing the `data.table`s directly through `x`, `y` and the
  three-dot syntax. If `dlist` is specified, `x`, `y` and all datasets
  parsed through `...` are ignored.

- by:

  A single character string specifying the name of the column which
  uniquely identifies individuals in the supplied datasets. The column
  identified by this string may contain any variable type. Contrary to
  the `by` argument of standard `merge` functions, this argument does
  not support multiple values. Since the goal is to merge on
  time-intervals, it is also necessary to additionaly define which
  columns identify them using the `start` and `stop` arguments.

- start:

  A single character string specifying the name of the `start` column in
  the supplied datasets. Defaults to `"start"`. The column identified by
  this string may contain and variable type that allows comparisons and
  that can be merged on (such as int, datetime, and floats).

- stop:

  A single character string specifying the name of the `stop` column in
  the supplied datasets. Defaults to `"stop"`. The column identified by
  this string may contain and variable type that allows comparisons and
  that can be merged on (such as int, datetime, and floats).

- all:

  Either `TRUE` or `FALSE` (default). `all=TRUE` is shorthand to save
  setting both `all.x=TRUE` and `all.y=TRUE`.

- all.x:

  Either `TRUE` or `FALSE`. If `TRUE`, cases from `x` (identified using
  the `by` column) which have no matching cases in `y` or any other
  dataset supplied afterwards through `...` are included in the output
  (if only `dlist` is specified, the first entry in `dlist` will be
  considered "x" and all other entries as "y"). These cases will have
  `NA`s in the columns that are usually filled with values from the
  other datasets. The default is `FALSE` so that only cases with data
  from both `x` and `y` (and potentially the other datasets) are
  included in the ouptut. This is analogous to the standard `merge`
  functions, with the only difference being that all datasets except `x`
  are counted as "y" datasets. These arguments only apply to cases
  identified using the `by` column, not to the time-intervals defined by
  `start` and `stop`.

- all.y:

  Either `TRUE` or `FALSE`, analogous to `all.x` above.

- first_time:

  An optional scalar value specifying the time at which the observation
  time starts for all individuals or `NULL` (default). If `NULL`, only
  the information given in the supplied data is used to construct the
  start-stop data. If specified, an additional time-period is added from
  `first_time` to the first event time of each case (if not present
  already). Note that this argument must be of the same type as the
  `start` and `stop` columns.

- last_time:

  Same as `first_time`, but for the end of the observation period.

- remove_before_first:

  Either `TRUE` (default) or `FALSE`. If `TRUE` and `first_time` is
  specified, all time intervals before `first_time` are removed for each
  individual. This is done by default if `first_time` is specified, but
  can be turned off by setting this argument to `FALSE`.

- remove_after_last:

  Either `TRUE` (default) or `FALSE`. If `TRUE` and `last_time` is
  specified, all time intervals after `last_time` are removed for each
  individual. This is done by default if `last_time` is specified, but
  can be turned off by setting this argument to `FALSE`.

- center_on_first:

  Either `TRUE` or `FALSE` (default), specifying whether the `start` and
  `stop` columns should be centered on the `first_time` value. If
  `first_time` is specified and `center_on_first=TRUE`, the first time
  period of each case will start at 0 and all periods afterwards will
  correspond to time elapsed since then.

- units:

  Corresponds to the `units` argument of the
  [`difftime`](https://rdrr.io/r/base/difftime.html) function. Only used
  when `start` and `stop` are of class `Date` (or something similar) and
  `center_on_first` is set to `TRUE`. Ignored otherwise.

- defaults:

  An optional named list containing default values for some or all
  variable or `NULL` (default). If `NULL`, missing information is kept
  as `NA`. This argument may be useful if the supplied datasets do not
  contain information for all of the time under observation for all
  individuals. The value in `defaults` will then be imputed for all such
  missing intervals. See details and examples.

- event_times:

  An optional `data.table` containing times at which an event occured
  for each case or `NULL` (default). If `NULL`, no events are added. If
  specified, the times included in this `data.table` are added to the
  resulting start-stop data in the form of a `status` variable. The data
  will then reflect what is usually needed to fit time-to-event or
  recurrent event models. See details and examples. The supplied
  `data.table` should include exactly two columns: `by` (see argument of
  the same name) and `time` (the time at which the event(s) occured).
  Multiple events per case are supported. Note that if `event_times`
  contains times before the first included time-period for a case, those
  are not be included in the output. Users should define the
  `first_time` argument accordingly, or provide full information on the
  required time-periods through the supplied datasets.

- time_to_first_event:

  Either `TRUE` or `FALSE` (default), specifying whether the observation
  time after the first event per case should be removed or not. If
  `TRUE`, only time until the first event per person is kept. Event
  times should be defined using the `event_times` argument. This
  argument is ignored if `event_times` is `NULL`. Note that if
  `first_time` is not specified and the first event of a case happens
  before the first included time period, the case will be removed
  entirely from the output.

- status:

  A single character string specifying how the status variable should be
  named. Defaults to `"status"`. Ignored if `event_times=NULL`.

- constant_vars:

  An optional `data.table` containing values of variables that are
  constant over time for some or all cases in the supplied data or
  `NULL` (default). If specified, the constant variables are simply
  merged to the final start-stop data. The input to `constant_vars`
  should therefore contain a column named as the `by` argument.

## Details

The start-stop format, also known as counting process or period format
corresponds to a `data.table` containing one or multiple rows per `by`,
where each row corresponds to a period of time in which no variables
changed. These intervals are defined by the `start` and `stop` columns.
The `start` column gives the time at which the period started, the
`stop` column denotes the time when the period ended. This type of
dataset is required for the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function and most of the existing time-to-event models with time-varying
variables, such as `coxph`. Bringing existing data on period information
into this format can become tricky and computationally expensive fast.
The purpose of this function is to make this transformation as fast and
easy as possible.

***Interval Coding for Time-Varying Variables***:

The intervals supplied to this function via `x, y, ...` or `dlist` and
the output created is based entirely on overlapping intervals. More
specifically, *right-open* intervals `[start, stop)` are expected and
used throughout it, which is the usual data format expected for
time-to-event modelling and corresponds to the interval format of the
`tmerge` function of the survival package. As a consequence, intervals
of length 0 (where `start==stop`) are not supported and will result in
an error message.

Note that this only applies to the time-varying variables themselves.
Events added using `event_times` are coded differently, as described
further below.

***Specifying the input***:

Each input `data.table` should include information about time-periods of
one or more variables. For example, suppose that we want to include data
for two time-varying variables for individual `id=1`. These variables
are `BMI` and `Treatment` and have been measured in different time
intervals. Suppose we have the following information for BMI:

      d1 <- data.table(id=c(1, 1),
                       start=c(0, 10),
                       stop=c(10, 22),
                       BMI=c(32.1, 35))

In other words, the BMI has a value of 32.1 in the interval `[0, 10)`
and a value of 35 ín the interval `[10, 22)`. Additionally, we know the
days on which a treatment was given:

      d2 <- data.table(id=c(1, 1, 1),
                       start=c(3, 6, 12),
                       stop=c(4, 7, 13),
                       Treatment=c(TRUE, TRUE, TRUE))

Here, only the actual days with a treatment are recorded, but it is
known that no treatment was given at all other times. We could include
this information directly into `d2` by adding the intervals inbetween,
but there is no need to do that, as we will see soon. The two datasets
could be merged into one start-stop dataset using:

      data <- merge_start_stop(dl, d2, by="id", defaults=list("Treatment"=FALSE))

This example is also given below if you want to run it yourself. By
specifying the `defaults` argument for "Treatment" to be `FALSE`, we get
the desired start-stop dataset. By using `...` or the `dlist` argument,
any number of `data.table`s with full information (such as `d1`) or
partial information (such as `d2`) may be merged. More examples are
given below.

***Inclusion of Event Times***:

The type of start-stop data required for most time-to-event models with
time-varying variables or recurrent events requires that event times are
coded differently than standard intervals. Instead of creating a new
interval for an event time, existing intervals should end at the event
times, effectively breaking up the existing intervals (see for example
Chiou et al. 2023). This can be achieved in this function by supplying
the event times separately using the `event_times` argument. Multiple
events per case are supported. If only the time until the first event
should be kept, users may set `time_to_first_event` to `TRUE`.

If `first_time` is specified and `remove_before_first=TRUE`, all events
occuring before `first_time` will be removed as well. If
`time_to_first_event=TRUE` is used additionally, all cases with events
before `first_time` will be removed entirely. If users want to instead
keep only the time until the first event *after* `first_time`, they are
required to remove all events before that point in time from the
`event_times` dataset before calling this function.

***Speed Considerations***:

Contrary to similar functions such as the `tmerge` function from the
survival package, this function does not rely on the `ivs` package to
perform the data transformation. It instead relies only on the
`data.table` package, which is highly optimized in terms of RAM usage
and performance. As such, this function scales a lot better with large
inputs (both large amount of cases and with many intervals). For small
datasets there should be no discernible difference between the
functions.

Note that supplying all datasets that should be merged at the same time
is faster than calling this function multiple times with one `x` and `y`
each, although it might be necessary to do the latter in certain
situations

## Value

Returns a single `data.table` containing all supplied information about
the intervals in the start-stop format.

## References

Sy Han Chiou, Gongjun Xu, Jun Yan, and Chiung-Yu Huang (2023).
"Regression Modeling for Recurrent Events Possibly with an Informative
Terminal Event Using R Package reReg". In: Journal of Statistical
Software. 105.5, pp. 1-34.

## Author

Robin Denz

## See also

[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)

## Examples

``` r
library(MatchTime)
library(data.table)
library(fastmatch)

## example from "Details" section
# NOTE: actual data may of course include more than one unique "id"
#       and more than two variables
d1 <- data.table(id=c(1, 1),
                 start=c(0, 10),
                 stop=c(10, 22),
                 BMI=c(32.1, 35))

d2 <- data.table(id=c(1, 1, 1),
                 start=c(3, 6, 12),
                 stop=c(4, 7, 13),
                 Treatment=c(TRUE, TRUE, TRUE))

# setting default for Treatment
data <- merge_start_stop(d1, d2, by="id", defaults=list("Treatment"=FALSE))
print(data)
#> Key: <id, start>
#>       id start  stop   BMI Treatment
#>    <num> <num> <num> <num>    <lgcl>
#> 1:     1     0     3  32.1     FALSE
#> 2:     1     3     4  32.1      TRUE
#> 3:     1     4     6  32.1     FALSE
#> 4:     1     6     7  32.1      TRUE
#> 5:     1     7    10  32.1     FALSE
#> 6:     1    10    12  35.0     FALSE
#> 7:     1    12    13  35.0      TRUE
#> 8:     1    13    22  35.0     FALSE

# setting no such defaults
data <- merge_start_stop(d1, d2, by="id")
print(data)
#> Key: <id, start>
#>       id start  stop   BMI Treatment
#>    <num> <num> <num> <num>    <lgcl>
#> 1:     1     0     3  32.1        NA
#> 2:     1     3     4  32.1      TRUE
#> 3:     1     4     6  32.1        NA
#> 4:     1     6     7  32.1      TRUE
#> 5:     1     7    10  32.1        NA
#> 6:     1    10    12  35.0        NA
#> 7:     1    12    13  35.0      TRUE
#> 8:     1    13    22  35.0        NA

# with a first_time and last_time argument
data <- merge_start_stop(d1, d2, by="id", first_time=-5, last_time=100)
print(data)
#> Key: <id, start>
#>        id start  stop   BMI Treatment
#>     <num> <num> <num> <num>    <lgcl>
#>  1:     1    -5     0    NA        NA
#>  2:     1     0     3  32.1        NA
#>  3:     1     3     4  32.1      TRUE
#>  4:     1     4     6  32.1        NA
#>  5:     1     6     7  32.1      TRUE
#>  6:     1     7    10  32.1        NA
#>  7:     1    10    12  35.0        NA
#>  8:     1    12    13  35.0      TRUE
#>  9:     1    13    22  35.0        NA
#> 10:     1    22   100    NA        NA

# last_time < actually observed last time
data <- merge_start_stop(d1, d2, by="id", last_time=17)
print(data)
#> Key: <id, start>
#>       id start  stop   BMI Treatment
#>    <num> <num> <num> <num>    <lgcl>
#> 1:     1     0     3  32.1        NA
#> 2:     1     3     4  32.1      TRUE
#> 3:     1     4     6  32.1        NA
#> 4:     1     6     7  32.1      TRUE
#> 5:     1     7    10  32.1        NA
#> 6:     1    10    12  35.0        NA
#> 7:     1    12    13  35.0      TRUE
#> 8:     1    13    17  35.0        NA

# first_time > actually observed first time
data <- merge_start_stop(d1, d2, by="id", first_time=8)
print(data)
#> Key: <id, start>
#>       id start  stop   BMI Treatment
#>    <num> <num> <num> <num>    <lgcl>
#> 1:     1     8    10  32.1        NA
#> 2:     1    10    12  35.0        NA
#> 3:     1    12    13  35.0      TRUE
#> 4:     1    13    22  35.0        NA

# adding a time-constant variable sex
d3 <- data.table(id=1, sex="female")
data <- merge_start_stop(d1, d2, by="id", constant_vars=d3)
print(data)
#> Key: <id>
#>       id start  stop   BMI Treatment    sex
#>    <num> <num> <num> <num>    <lgcl> <char>
#> 1:     1     0     3  32.1        NA female
#> 2:     1     3     4  32.1      TRUE female
#> 3:     1     4     6  32.1        NA female
#> 4:     1     6     7  32.1      TRUE female
#> 5:     1     7    10  32.1        NA female
#> 6:     1    10    12  35.0        NA female
#> 7:     1    12    13  35.0      TRUE female
#> 8:     1    13    22  35.0        NA female

# adding event times
d4 <- data.table(id=c(1, 1), time=c(4, 31))
data <- merge_start_stop(d1, d2, by="id", event_times=d4)
print(data)
#> Key: <id>
#>       id start  stop   BMI Treatment status
#>    <num> <num> <num> <num>    <lgcl> <lgcl>
#> 1:     1     0     3  32.1        NA  FALSE
#> 2:     1     3     4  32.1      TRUE   TRUE
#> 3:     1     4     6  32.1        NA  FALSE
#> 4:     1     6     7  32.1      TRUE  FALSE
#> 5:     1     7    10  32.1        NA  FALSE
#> 6:     1    10    12  35.0        NA  FALSE
#> 7:     1    12    13  35.0      TRUE  FALSE
#> 8:     1    13    22  35.0        NA  FALSE
#> 9:     1    22    31    NA        NA   TRUE
```
