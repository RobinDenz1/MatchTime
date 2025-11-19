# Transform a long-format dataset into the start-stop format

This function transforms a `data.table` like object in the long-format
(one row per person per time point) to a `data.table` in the start-stop
format (one row per person-specific period in which no variables
changed).

## Usage

``` r
long2start_stop(data, id, time, varying, start_name="start",
                stop_name="stop")
```

## Arguments

- data:

  A `data.table` or an object that can be coerced to a `data.table`
  (such as a `data.frame`) including data in the long-format. The
  supplied `data` should include full information for all individuals,
  e.g. no points in time between the first time and last time per
  individual should be missing.

- id:

  A single character string specifying a unique person identifier
  included in in `data`.

- time:

  A single character string specifying a time variable included in in
  `data` coded as integers.

- varying:

  A character vector specifying names of variables included in in `data`
  that may change over time.

- start_name:

  A single character string, specifying the name that the `"start"`
  variable should have in the resulting data. Defaults to `"start"`.

- stop_name:

  Same as `start_name` but for the `"stop"` column.

## Details

The created start-stop columns are coded as `[start, stop)`, meaning
that they will be overlapping. If non-overlapping intervals are desired,
users can simply substract 1 from the `stop` column.

This function relies on `data.table` syntax to make the data
transformation as RAM efficient and fast as possible.

## Value

Returns a `data.table` containing the columns `id` (the unique person
identifier), `start_name` (an integer variable encoding the beginning of
the intervals), `stop` (an integer variable encoding the end of the
intervals) and all other variables included in the input `data` in the
start-stop format.

## Author

Robin Denz

## Examples

``` r
library(MatchTime)
library(data.table)

# generate example data in long format
long <- data.table(.id=rep(seq_len(10), each=5),
                   .time=rep(seq_len(5), 10),
                   A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                           TRUE),
                   B=FALSE)
setkey(long, .id, .time)

# transform to start-stop format
long2start_stop(data=long, id=".id", time=".time", varying=c("A", "B"))
#>       .id start  stop      A      B
#>     <int> <int> <int> <lgcl> <lgcl>
#>  1:     1     1     6  FALSE  FALSE
#>  2:     2     1     6  FALSE  FALSE
#>  3:     3     1     6  FALSE  FALSE
#>  4:     4     1     6  FALSE  FALSE
#>  5:     5     1     6  FALSE  FALSE
#>  6:     6     1     6  FALSE  FALSE
#>  7:     7     1     6  FALSE  FALSE
#>  8:     8     1     6  FALSE  FALSE
#>  9:     9     1     4  FALSE  FALSE
#> 10:     9     4     6   TRUE  FALSE
#> 11:    10     1     4  FALSE  FALSE
#> 12:    10     4     6   TRUE  FALSE
```
