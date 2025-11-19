# Combines rows with the same values in start-stop data

Given a `data.table`-like object containing information in the
start-stop format, this function searches for consecutive intervals
where values of specific covariates do not change and "simplifies" the
dataset by combining these interval into one interval. This may be
useful to reduce RAM usage and computation time when dealing with large
start-stop data.

## Usage

``` r
simplify_start_stop(data, id, start="start", stop="stop",
                    cols, remove_other_cols=TRUE)
```

## Arguments

- data:

  A `data.table` like object including at least four columns: `id` (the
  case identifier), `start` (the beginning of the time-interval), `stop`
  (the end of the time-interval) and one or more arbitrary columns. May
  also be any object that can be coerced to be a `data.table`, such as a
  `data.frame` or a `tibble`. Intervals should be *right-open* (e.g.
  coded as `[start, stop)`).

- id:

  A single character string specifying a column in `data` containing the
  case identifiers.

- start:

  A single character string specifying a column in `data` specifying the
  beginning of a time-interval. Defaults to `"start"`.

- stop:

  A single character string specifying a column in `data` specifying the
  ending of a time-interval. Defaults to `"stop"`.

- cols:

  A character vector specifying the columns that should be used to check
  whether the intervals are unique. If not specified, all columns other
  than `id`, `start` and `stop` will be used.

- remove_other_cols:

  Either `TRUE` or `FALSE`, specifying whether the columns *not* named
  in the `cols` argument (other than `id`, `start`, `stop`) should be
  removed from the output. Defaults to `TRUE`, because keeping these
  columns may be misleading. If set to `FALSE`, please remember that the
  value of these columns is not neccesarily correct, since intervals
  have been combined without looking at their values first.

## Details

The intervals defined by the `start` and `stop` columns are expected to
be coded as `[start, stop)`, meaning that the value of `start` must
always be equal to the value of `stop` in the previous row. Intervals of
length 0 are not supported and will produce an error message.

Note that if the input `data` contains events, users probably want to
exclude these event columns from the `cols` argument. The reason is that
the `data` may contain consecutive intervals that are indeed exactly the
same, but refer to two separate events (because intervals always end
when an event indicator is `TRUE`).

## Value

Returns a single `data.table` containing the simplified dataset.

## Author

Robin Denz

## See also

[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md),
[`fill_gaps_start_stop`](https://robindenz1.github.io/MatchTime/reference/fill_gaps_start_stop.md),
[`subset_start_stop`](https://robindenz1.github.io/MatchTime/reference/subset_start_stop.md)

## Examples

``` r
library(MatchTime)
library(data.table)

# get some fake example data
data1 <- data.table(id=1,
                    start=c(1, 20, 35, 120, 923, 1022, 2000, 3011),
                    stop=c(20, 35, 120, 923, 1022, 2000, 3011, 3013),
                    A=c(0, 0, 0, 1, 1, 0, 0, 0),
                    B=c(1, 0, 0, 1, 0, 0, 0, 0),
                    C=c(11, 0.2, 17.8, 2.1, 9.0001, 1.2, 33, 22))
data2 <- data.table(id=2,
                    start=c(1, 20, 35, 120, 923),
                    stop=c(20, 35, 120, 923, 1022),
                    A=c(0, 0, 1, 1, 1),
                    B=c(1, 0, 0, 1, 0),
                    C=c(11, 0.2, 17.8, 2.1, 9.0001)+1)
data <- rbind(data1, data2)

# simplify in regards to columns "A" and "B"
out <- simplify_start_stop(data, id="id", cols=c("A", "B"))
print(out)
#> Key: <id, start>
#>        id start  stop     A     B
#>     <num> <num> <num> <num> <num>
#>  1:     1     1    20     0     1
#>  2:     1    20   120     0     0
#>  3:     1   120   923     1     1
#>  4:     1   923  1022     1     0
#>  5:     1  1022  3013     0     0
#>  6:     2     1    20     0     1
#>  7:     2    20    35     0     0
#>  8:     2    35   120     1     0
#>  9:     2   120   923     1     1
#> 10:     2   923  1022     1     0

# simplify in regards to column "A" only
out <- simplify_start_stop(data, id="id", cols="A")
print(out)
#> Key: <id, start>
#>       id start  stop     A
#>    <num> <num> <num> <num>
#> 1:     1     1   120     0
#> 2:     1   120  1022     1
#> 3:     1  1022  3013     0
#> 4:     2     1    35     0
#> 5:     2    35  1022     1

# calling it without specifying "cols" results in no changes,
# because C always changes over the defined intervals
out <- simplify_start_stop(data, id="id")
print(out)
#> Key: <id, start>
#>        id start  stop     A     B       C
#>     <num> <num> <num> <num> <num>   <num>
#>  1:     1     1    20     0     1 11.0000
#>  2:     1    20    35     0     0  0.2000
#>  3:     1    35   120     0     0 17.8000
#>  4:     1   120   923     1     1  2.1000
#>  5:     1   923  1022     1     0  9.0001
#>  6:     1  1022  2000     0     0  1.2000
#>  7:     1  2000  3011     0     0 33.0000
#>  8:     1  3011  3013     0     0 22.0000
#>  9:     2     1    20     0     1 12.0000
#> 10:     2    20    35     0     0  1.2000
#> 11:     2    35   120     1     0 18.8000
#> 12:     2   120   923     1     1  3.1000
#> 13:     2   923  1022     1     0 10.0001
```
