# Add information from start-stop data to a `match_time` object

This function may be used to add variables saved in the start-stop
format to a `match_time` object created using the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
function. For example, there might be a time-varying variable that was
not included in the original `data` used in the
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
call to save memory, that should now be added later. Using this
function, the value of these variables at `.treat_time` will be added to
the `data` in the `match_time` object.

## Usage

``` r
add_from_start_stop(x, data, variable, id=x$id,
                    start="start", stop="stop", default=NA)
```

## Arguments

- x:

  A `match_time` object created using the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- data:

  A `data.table` like object including at least four columns: `id` (the
  unique case identifier), `start` (beginning of interval), `stop` (end
  of interval) and one or more columns defined by the `variables`
  argument. May also be any object that can be coerced to be a
  `data.table`, such as a `data.frame` or a `tibble`. The dataset should
  be in the same start-stop format that would be required for a regular
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  call.

- id:

  A single character string specifying a column in `data`, specifying
  the unique case identifier. By default the same name that was used in
  the original
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  is used here.

- variable:

  A single character string specifying a variable in `data` which should
  be added to the `$data` object inside of `x`. Note that if `x$data`
  already contains columns with the same names as in `variables`, an
  error will be produced.

- start:

  A single character string specifying a column in `data` specifying the
  beginning of a time-interval. Defaults to `"start"`.

- stop:

  A single character string specifying a column in `data` specifying the
  end of a time-interval. Defaults to `"stop"`.

- default:

  A single value that should be used as default if no information can be
  found in `data` for the required point in time for some individuals.
  If the supplied `data` does not contain information for some `id` at
  `.treat_time`, this default value will be used instead.

## Details

Although this function may be useful for very large datasets, if RAM is
not an issue we recommend using the
[`merge_start_stop`](https://robindenz1.github.io/MatchTime/reference/merge_start_stop.md)
or similar functions to create a single start-stop dataset to use in
[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
instead. This way, all information contained in the start-stop dataset
is automatically included in the output as well, without the need to use
this function.

## Value

Returns a modified `match_time` object. It is essentially the same
object as the supplied `x`, but it also contains a new column: `name`
(the time of occurence of the next event).

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

}
```
