# Fast Stratified Sampling from a `data.table` like object

This function may be used to draw a stratified random sample (with or
without replacement) of rows from a `data.table` or similar object. It
additionally includes options on how to handle cases in which the number
to be drawn is less than the number of actual rows when using
`replace=FALSE`, which makes it useful in exact matching.

## Usage

``` r
stratified_sample(data, n, strata, replace=FALSE,
                  max_replace=NULL, if_lt_n="stop")
```

## Arguments

- data:

  A `data.table` like object that should be sampled from. May also be
  any object that can be coerced to be a `data.table`, such as a
  `data.frame` or a `tibble`.

- n:

  A named numeric vector containing values \> 0, specifying the number
  of rows that should be sampled for each value in `strata`.

- strata:

  A single character string specifying the integer, character or factor
  variable representing the strata that should be sampled from in
  `data`. Only supports a single variable. If data should be sampled
  from strata of multiple variables, the user needs to first create a
  single variable from these multiple variables as shown in the
  examples.

- replace:

  Either `TRUE` or `FALSE` (default). Specifies whether the sampling
  should be performed with or without replacement.

- max_replace:

  Either `NULL` (default) or a named numeric vector containing values \>
  0, specifying the number of times that rows in each value of `strata`
  may be replace when using `replace=TRUE`. Ignored if `replace=FALSE`.
  Keeping this argument at `NULL` results in no upper limit on
  replacement of rows per `strata`.

- if_lt_n:

  Must be either `"stop"`, `"warn"` or `"nothing"`. Controls whether to
  throw an error, a warning or silently accept when the number of rows
  in a stratum specified by `strata` is smaller than the number that
  should be sampled from it and `replace=FALSE` is used.

## Details

Internally splits the `data.table` into parts as defined by the `strata`
variable and then uses the
[`sample.int`](https://rdrr.io/r/base/sample.html) function to sample
rows from these strata. By using only `data.table` functions and
[`sample.int`](https://rdrr.io/r/base/sample.html) instead of `sample`,
this function is very efficient even for very large `data.tables` and
many possible strata.

## Value

Returns a single `data.table` containing the sampled rows.

## References

Lohr, Sharon L. (2010). Sampling: Design and Analysis. Bd. 2. Boston:
Cengage Learning.

## Author

Robin Denz

## See also

[`fast_exact_matching`](https://robindenz1.github.io/MatchTime/reference/fast_exact_matching.md)

## Examples

``` r
library(MatchTime)
library(data.table)
library(fastmatch)

set.seed(344)

data <- data.table(A=stats::rnorm(n=100),
                   B=sample(c("A", "B", "C"), size=100, replace=TRUE),
                   strat=sample(c("1", "2", "3"), size=100, replace=TRUE))

n <- c(10, 25, 31)
names(n) <- c("2", "1", "3")

# sample (without replacement):
# - 25 rows from strat=="1"
# - 10 rows from strat=="2"
# - 31 rows from strat=="3"
samp <- stratified_sample(data, n=n, strata="strat")

# take the same stratified sample with replacement
samp <- stratified_sample(data, n=n, strata="strat", replace=TRUE)
```
