# Add missing intervals to incomplete start-stop data

Some start-stop datasets may be missing some intervals for some cases.
This function adds those intervals to the data, which might be needed
for further processing.

## Usage

``` r
fill_gaps_start_stop(data, id, start="start", stop="stop",
                     first_time=NULL, last_time=NULL,
                     missing_indicator=TRUE, ...)
```

## Arguments

- data:

  A `data.table` like object including at least three columns: `id` (the
  unique case identifier), `start` (the beginning of the time-interval)
  and `stop` (the end of the time-interval). May also be any object that
  can be coerced to be a `data.table`, such as a `data.frame` or a
  `tibble`. Intervals should be coded as `(start, stop]`.

- id:

  A single character string specifying a column in `id` specifying the
  unique case identifier.

- start:

  A single character string specifying a column in `data` specifying the
  beginning of a time-interval. Defaults to `"start"`.

- stop:

  A single character string specifying a column in `data` specifying the
  ending of a time-interval. Defaults to `"stop"`.

- first_time:

  Corresponds to the argument of the same name in the
  [`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
  function.

- last_time:

  Corresponds to the argument of the same name in the
  [`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
  function.

- missing_indicator:

  Either `TRUE` (default) or `FALSE`, specifying whether a logical
  column should be added to the data which includes an indicator whether
  the corresponding interval was present in the original data or not. If
  `TRUE`, this column will be added using the name `".in_data"`. This
  column is `TRUE` if the interval was present and `FALSE` if it was
  not.

- ...:

  Further arguments passed to the
  [`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
  function.

## Details

If the input data does not have any missing intervals, the input stays
unchanged. If missing intervals are added, they have a value of `NA` in
all columns other than `start`, `stop` and `id`.

Internally, this function simply calls the
[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
function with appropriate input argument (merging the input `data` with
an essentially empty second dataset using `all=TRUE`). As such, all
arguments of the
[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
function, such as setting default values using the `defaults` argument
may also be used here.

## Value

Returns a single `data.table` containing the start-stop data with all
missing intervals added.

## Author

Robin Denz

## See also

[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md),
[`subset_start_stop`](https://robindenz1.github.io/MatchTime/reference/subset_start_stop.md),
[`simplify_start_stop`](https://robindenz1.github.io/MatchTime/reference/simplify_start_stop.md)

## Examples

``` r
library(MatchTime)
library(data.table)

## define some example data
# this dataset is missing two intervals: (24, 28] for id = 1 and
# (245, 343] for id = 2
data <- data.table(id=c(1, 1, 1, 2, 2, 3),
                   start=c(0, 28, 66, 25, 343, 10),
                   stop=c(24, 66, 143, 245, 1233, 3214),
                   A=c(10, 122, 3434, 223, 233, 0.46))

# add those intervals back
out <- fill_gaps_start_stop(data, id="id")
print(out)
#> Key: <id, start>
#>       id start  stop .in_data       A
#>    <num> <num> <num>   <lgcl>   <num>
#> 1:     1     0    24     TRUE   10.00
#> 2:     1    24    28    FALSE      NA
#> 3:     1    28    66     TRUE  122.00
#> 4:     1    66   143     TRUE 3434.00
#> 5:     2    25   245     TRUE  223.00
#> 6:     2   245   343    FALSE      NA
#> 7:     2   343  1233     TRUE  233.00
#> 8:     3    10  3214     TRUE    0.46

# add the missing intervals, but also add missing intervals from 0 to first
# observed value per id
out <- fill_gaps_start_stop(data, id="id", first_time=0)
print(out)
#> Key: <id, start>
#>        id start  stop .in_data       A
#>     <num> <num> <num>   <lgcl>   <num>
#>  1:     1     0    24     TRUE   10.00
#>  2:     1    24    28    FALSE      NA
#>  3:     1    28    66     TRUE  122.00
#>  4:     1    66   143     TRUE 3434.00
#>  5:     2     0    25    FALSE      NA
#>  6:     2    25   245     TRUE  223.00
#>  7:     2   245   343    FALSE      NA
#>  8:     2   343  1233     TRUE  233.00
#>  9:     3     0    10    FALSE      NA
#> 10:     3    10  3214     TRUE    0.46
```
