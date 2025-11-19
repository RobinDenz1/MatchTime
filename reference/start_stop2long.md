# Transform a start-stop dataset into the long-format

Given a `data.table` like object in the start-stop format, this function
returns a `data.table` in the long-format.

## Usage

``` r
start_stop2long(data, id, events=NULL, start="start",
                stop="stop", fill_gaps=FALSE,
                include_last_t=FALSE, time_name="time",
                ...)
```

## Arguments

- data:

  A `data.table` like object including at least three columns: `id` (the
  unique case identifier), `start` (the beginning of the time-interval)
  and `stop` (the end of the time-interval). May also be any object that
  can be coerced to be a `data.table`, such as a `data.frame` or a
  `tibble`. Intervals should be *right-open* (coded as `[start, stop)`)
  and thus overlapping . May contain any number of additional columns.

- id:

  A single character string specifying a column in `data` specifying the
  unique case identifier.

- events:

  Either `NULL` (default) or a character vector specifying variable
  names in `data`. The columns specified by this argument should be
  logical and are considered to be event indicators, meaning that they
  are not coded as time-varying variables. Instead they should be coded
  as occurring exactly on `stop` and have no duration themselves. In the
  long-format output, these columns will only be `TRUE` on the time of
  occurrence, not during the interval in which they were coded.

- start:

  A single character string specifying a column in `data` specifying the
  beginning of a time-interval. Defaults to `"start"`.

- stop:

  A single character string specifying a column in `data` specifying the
  ending of a time-interval. Defaults to `"stop"`.

- fill_gaps:

  Either `TRUE` or `FALSE` (default), specifying whether intervals that
  are missing from `data` should still be present in the output. If set
  to `TRUE`, the
  [`fill_gaps_start_stop`](https://robindenz1.github.io/MatchTime/reference/fill_gaps_start_stop.md)
  function is called on the input `data` first.

- include_last_t:

  Whether to include the last value of `stop` per `id` in the output.
  Whether this should be done or not depends on how the intervals are
  coded.

- time_name:

  A single character string, specifying the name of the "time" column in
  the output.

- ...:

  Further arguments passed to
  [`fill_gaps_start_stop`](https://robindenz1.github.io/MatchTime/reference/fill_gaps_start_stop.md)
  if `fill_gaps=TRUE`, ignored otherwise.

## Value

Returns a single `data.table` containing the long-format data. The
`start` and `stop` columns from the input are replaced by a single
`time_name` column.

## Author

Robin Denz

## See also

[`fill_gaps_start_stop`](https://robindenz1.github.io/MatchTime/reference/fill_gaps_start_stop.md),
[`long2start_stop`](https://robindenz1.github.io/MatchTime/reference/long2start_stop.md)

## Examples

``` r
library(MatchTime)
library(data.table)

# define some example start-stop data
data <- data.table(id=c(1, 1, 1, 2, 2, 3),
                   start=c(0, 14, 26, 0, 18, 0),
                   stop=c(14, 26, 30, 18, 32, 51),
                   A=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                   B=c(1L, 1L, 2L, 3L, 5L, 6L),
                   C=c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                   D=c("A", "B", "C", "D", "E", "F"))

# transform to long-format
out <- start_stop2long(data, id="id")
head(out)
#> Key: <id, time>
#>       id     A     B      C      D  time
#>    <num> <num> <int> <lgcl> <char> <int>
#> 1:     1   0.1     1   TRUE      A     0
#> 2:     1   0.1     1   TRUE      A     1
#> 3:     1   0.1     1   TRUE      A     2
#> 4:     1   0.1     1   TRUE      A     3
#> 5:     1   0.1     1   TRUE      A     4
#> 6:     1   0.1     1   TRUE      A     5

# if C was coded as an event instead, we would want to use:
out <- start_stop2long(data, id="id", events="C")
head(out)
#> Key: <id, time>
#>       id  time     A     B      D      C
#>    <num> <int> <num> <int> <char> <lgcl>
#> 1:     1     0   0.1     1      A  FALSE
#> 2:     1     1   0.1     1      A  FALSE
#> 3:     1     2   0.1     1      A  FALSE
#> 4:     1     3   0.1     1      A  FALSE
#> 5:     1     4   0.1     1      A  FALSE
#> 6:     1     5   0.1     1      A  FALSE
```
