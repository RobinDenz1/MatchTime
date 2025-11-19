# Subsetting start-stop format datasets

Returns subsets of a `data.table` like object in the start-stop format.
Contrary to the usual
[`subset`](https://rdatatable.gitlab.io/data.table/reference/subset.data.table.html)
function, this function subsets (and truncates) specific
*time-intervals* and does not use a logical expression to subset data
based on other column values. May be useful to limit start-stop based
datasets to a certain time-range.

## Usage

``` r
subset_start_stop(data, first_time, last_time,
                  truncate=TRUE, start="start",
                  stop="stop", na.rm=FALSE)
```

## Arguments

- data:

  A `data.table` like object including at least two columns: `start`
  (the beginning of the time-interval) and `stop` (the end of the
  time-interval). May also be any object that can be coerced to be a
  `data.table`, such as a `data.frame` or a `tibble`. Intervals should
  be coded as `[start, stop)`, like in all other functions of this
  package.

- first_time:

  A single value or a vector of size `nrow(data)` of class `numeric`,
  `Date` or something similar, specifying the first time that should be
  kept in the output. All intervals ending before this value will be
  removed. Additionally, if `truncate=TRUE`, all intervals starting
  before `first_time` and ending after `first_time` will be truncated to
  start at `first_time`.

- last_time:

  A single value or a vector of size `nrow(data)` of class `numeric`,
  `Date` or something similar, specifying the last time that should be
  kept in the output. All intervals beginning before this value will be
  removed. Additionally, if `truncate=TRUE`, all intervals starting
  before `last_time` and ending after `last_time` will be truncated to
  end at `last_time`.

- truncate:

  Either `TRUE` or `FALSE`, controls whether existing intervals should
  be truncated at `first_time` and or `last_time`. See the respective
  arguments for more info.

- start:

  A single character string specifying a column in `data` specifying the
  beginning of a time-interval. Defaults to `"start"`.

- stop:

  A single character string specifying a column in `data` specifying the
  ending of a time-interval. Defaults to `"stop"`.

- na.rm:

  Either `TRUE` or `FALSE` (default), controls whether to remove rows
  where either `first_time` or `last_time` is `NA`.

## Value

Returns a single `data.table` containing the subsetted start-stop data.

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
                   some_col=c(1, 2, 3, 4, 5, 6, 7, 8))

# limit it to the time-range 28 - 1900
out <- subset_start_stop(data, first_time=28, last_time=1900)
print(out)
#>       id start  stop some_col
#>    <num> <num> <num>    <num>
#> 1:     1    28   812        3
#> 2:     1   812  1092        4
#> 3:     1  1092  1900        5
#> 4:     2    90  1900        6

# don't truncate intervals
out <- subset_start_stop(data, first_time=28, last_time=1900,
                         truncate=FALSE)
print(out)
#>       id start  stop some_col
#>    <num> <num> <num>    <num>
#> 1:     1    25   812        3
#> 2:     1   812  1092        4
#> 3:     1  1092 34334        5
#> 4:     2    90  8021        6

# only cut-off intervals before t = 28
out <- subset_start_stop(data, first_time=28)
print(out)
#>       id start   stop some_col
#>    <num> <num>  <num>    <num>
#> 1:     1    28    812        3
#> 2:     1   812   1092        4
#> 3:     1  1092  34334        5
#> 4:     2    90   8021        6
#> 5:     2  9023   9823        7
#> 6:     2 10000 220022        8

# only cut-off intervals after t = 28
out <- subset_start_stop(data, last_time=28)
print(out)
#>       id start  stop some_col
#>    <num> <num> <num>    <num>
#> 1:     1     0    10        1
#> 2:     1    10    25        2
#> 3:     1    25    28        3

# using different cut-off values for each person
# note that we have to repeat the respective cut-off values as many times
# as each id appears to make this work
out <- subset_start_stop(data, last_time=c(rep(723, 5), rep(815, 3)))
print(out)
#>       id start  stop some_col
#>    <num> <num> <num>    <num>
#> 1:     1     0    10        1
#> 2:     1    10    25        2
#> 3:     1    25   723        3
#> 4:     2    90   815        6
```
